module Main where

import           S32EFV

import           Data.Maybe   (mapMaybe)
import qualified Data.Text.IO as TIO
import           System.IO

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


runServer :: IO ()
runServer = undefined

main :: IO ()
main = runInteractive 12
