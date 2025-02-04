module Main where

import           S32EFV

import           Control.Monad              (forM_, void)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B8
import           Data.Maybe                 (fromJust, mapMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Database.PostgreSQL.Simple
import           Network.HTTP.Types.Status
import           Network.Wai.Parse
import           Options.Applicative
import           System.Environment
import           System.IO
import           Web.Scotty                 hiding (Options, header)

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
    forM_ (map (TL.toStrict . TLE.decodeUtf8 . fileContent . snd) uploaded) $ \contents ->
      liftIO $ storeExpenses sh skip contents
    status status201

mkPostgresStorageHandle :: IO StorageHandle
mkPostgresStorageHandle = do
  connStr <- getEnv "DB_URL"
  conn <- connectPostgreSQL $ B8.pack connStr
  return $ mkStorageHandle (_storeExpenses conn) (_getExpenses conn)
  where
    _getExpenses conn = do
       rs <- query_ conn "select date, amount, tipo_raw, tipo, reference from expenses"
       return $ map tupleToExpense rs

    _storeExpenses conn cls =
      void $ executeMany conn (mconcat
        [ "insert into expenses (date, amount, tipo_raw, tipo, reference) "
        , "values (?, ?, ?, ?, ?)"
        ]) (map expenseToTuple cls)

    tupleToExpense (d, a, tr, "want", r) = Wants $ MkMovement d tr a r
    tupleToExpense (d, a, tr, "need", r) = Needs $ MkMovement d tr a r
    tupleToExpense (_, _, _ , t,      _) = error $ "invalid expense type: " <> t

    expenseToTuple :: Classification -> (ByteString, Double, ByteString, ByteString, ByteString)
    expenseToTuple e@(Wants (MkMovement d tr _ r)) = (TE.encodeUtf8 . toTimestamp $ d, fromJust $ toFloat e, TE.encodeUtf8 tr, "want", TE.encodeUtf8 r)
    expenseToTuple e@(Needs (MkMovement d tr _ r)) = (TE.encodeUtf8 . toTimestamp $ d, fromJust $ toFloat e, TE.encodeUtf8 tr, "need", TE.encodeUtf8 r)

    toTimestamp t =
      let (dd:mm:yyyy:_) = T.splitOn "/" t
       in yyyy <> "-" <> mm <> "-" <> dd


data Options = Options
  { optCommand :: !Command
  }

data Command
  = Interactive InteractiveOptions
  | Serve ServeOptions

data InteractiveOptions = InteractiveOptions
  { interactiveOptSkipLines :: !Int
  }

data ServeOptions = ServeOptions
  { serveOptSkipLines :: !Int
  }

interactiveCommand :: Parser Command
interactiveCommand = Interactive <$> InteractiveOptions
  <$> option auto
    ( long "skip"
    <> short 's'
    <> showDefault
    <> value 12
    <> metavar "INT"
    )

serveCommand :: Parser Command
serveCommand = Serve <$> ServeOptions
    <$> option auto
      ( long "skip"
      <> short 's'
      <> showDefault
      <> value 12
      <> metavar "INT"
      )

optParser :: Parser Options
optParser = Options
  <$> subparser
    ( command "interact" (info interactiveCommand (progDesc "Run the program in interactive mode") )
   <> command "serve" (info serveCommand (progDesc "Run an HTTP server") )
    )

opts :: ParserInfo Options
opts = info (optParser <**> helper)
  ( fullDesc
  <> progDesc "Calculate your expenses according to the 50/30/20 scheme"
  <> header "S32EFV - a tool to help you keep track of your expenses"
  )

main :: IO ()
main = do
  opts' <- execParser opts
  case optCommand opts' of
    Interactive iOpts -> runInteractive (interactiveOptSkipLines iOpts)
    Serve sOpts       -> do
      sh <- mkPostgresStorageHandle
      runServer sh (serveOptSkipLines sOpts)
