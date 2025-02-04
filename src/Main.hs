module Main where

import           S32EFV

import           Control.Monad             (forM_)
import           Data.Maybe                (mapMaybe)
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TE
import           Network.HTTP.Types.Status
import           Network.Wai.Parse
import           System.IO
import           Web.Scotty

runInteractive :: Int -> IO ()
runInteractive toSkip = print . aggregate . mapMaybe id =<< go 0 []
  where
    go n cls | n >= toSkip = do
      closed <- isEOF
      if closed then pure cls
      else do
        cls' <- (classify <$> TIO.getLine)
        go n (cls' : cls)
    go n cls = TIO.getLine >> go (n + 1) cls

runServer :: StorageHandle -> Int -> IO ()
runServer sh skip = scotty 3000 $ do
  get "/api/expenses" $ do
    -- Returns a list of all expenses
    expenses <- liftIO $ retrieveExpenses sh
    json expenses

  post "/api/expenses" $ do
    -- Upload a file with expenses
    uploaded <- files
    forM_ (map (TL.toStrict . TE.decodeUtf8 . fileContent . snd) uploaded) $ \contents ->
      liftIO $ storeExpenses sh skip contents
    status status201

mkPostgresStorageHandle :: IO StorageHandle
mkPostgresStorageHandle = pure $ mkStorageHandle undefined undefined

defaultSkipLines :: Int
defaultSkipLines = 12

main :: IO ()
main = do
  sh <- mkPostgresStorageHandle
  runServer sh defaultSkipLines
