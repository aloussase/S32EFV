module S32EFV.Parse.BancoBolivariano (mkBancoBolivarianoParseHandle) where

import           S32EFV.Parse.Handle
import           S32EFV.Types

import           Control.Monad        (guard)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char (char)

type Parser = Parsec Void Text

mkBancoBolivarianoParseHandle :: ParseHandle
mkBancoBolivarianoParseHandle = mkParseHandle "banco-bolivariano" $ \line ->
  let (parser :: Parser Movement) = do
        parts <- (T.pack <$> many (anySingleBut ',')) `sepBy` char ','
        guard (length parts >= 11)

        let tipo =
              if parts !! 6 == "(-)"
              then "nota debito"
              else "nota credito"

        return $ MkMovement
         { mDate = parts !! 1
         , mTipo = tipo
         , mMonto = parts !! 7
         , mRef = parts !! 10
         }
  in parseMaybe parser line



