module Options where

import           Control.Monad       (guard)
import           Data.List           (intercalate)
import qualified Data.Text           as T
import           Options.Applicative
import           S32EFV

data Options = Options
  { optCommand     :: !Command
  , optParseHandle :: !ParseHandle
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

parseHandleReader :: String -> Maybe ParseHandle
parseHandleReader str = do
  let str' = T.pack str
  guard (str' `elem` map getIdentifier parsers)
  return $ head $ filter (\p -> getIdentifier p == str') parsers

optParser :: Parser Options
optParser = Options
  <$> subparser
    ( command "interact" (info interactiveCommand (progDesc "Run the program in interactive mode") )
   <> command "serve" (info serveCommand (progDesc "Run an HTTP server") )
    )
  <*> option (maybeReader parseHandleReader)
      ( long "parser"
      <> short 'p'
      <> help ("One of " <> intercalate "," (map (T.unpack . getIdentifier) parsers))
      )

opts :: ParserInfo Options
opts = info (optParser <**> helper)
  ( fullDesc
  <> progDesc "Calculate your expenses according to the 50/30/20 scheme"
  <> header "S32EFV - a tool to help you keep track of your expenses"
  )
