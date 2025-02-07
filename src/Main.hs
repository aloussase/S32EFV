module Main where

import           Options
import           S32EFV

import           Control.Monad             (forM_)

import           Data.Maybe                (mapMaybe)
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE
import           Network.HTTP.Types.Status
import           Network.Wai.Parse
import           Options.Applicative
import           System.IO
import           Web.Scotty                hiding (Options, header)

runInteractive :: ParseHandle -> Int -> IO ()
runInteractive ph toSkip = print . aggregate . mapMaybe id =<< go 0 []
  where
    go n cls | n >= toSkip = do
      closed <- isEOF
      if closed then pure cls
      else do
        cls' <- (classify ph <$> TIO.getLine)
        go n (cls' : cls)
    go n cls = TIO.getLine >> go (n + 1) cls

runServer :: ParseHandle -> StorageHandle -> Int -> IO ()
runServer ph sh skip = scotty 3000 $ do
  get "/api/expenses" $ do
    -- Returns a list of all expenses
    expenses <- liftIO $ retrieveExpenses sh
    json expenses

  post "/api/expenses" $ do
    -- Upload a file with expenses
    uploaded <- files
    forM_ (map (TL.toStrict . TLE.decodeUtf8 . fileContent . snd) uploaded) $ \contents ->
      liftIO $ storeExpenses ph sh skip contents
    status status201

main :: IO ()
main = do
  opts' <- execParser opts
  case optCommand opts' of
    Interactive iOpts -> runInteractive (optParseHandle opts')(interactiveOptSkipLines iOpts)
    Serve sOpts       -> do
      sh <- mkPostgresStorageHandle
      runServer (optParseHandle opts') sh (serveOptSkipLines sOpts)
