module S32EFV.Parse.BancoGuayaquil (mkBancoGuayaquilParseHandle) where

import           S32EFV.Parse.Handle
import           S32EFV.Types

import           Control.Monad        (guard)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char (char)

type Parser = Parsec Void Text

mkBancoGuayaquilParseHandle :: ParseHandle
mkBancoGuayaquilParseHandle = mkParseHandle "banco-guayaquil" $ \line ->
  let (parser :: Parser Movement) = do
        parts <- (T.pack <$> many (anySingleBut '\t')) `sepBy` char '\t'
        guard (length parts >= 11)
        return $ MkMovement
         { mDate = parts !! 1
         , mTipo = parts !! 3
         , mMonto = parts !! 7
         , mRef = parts !! 10
         }
  in parseMaybe parser line
