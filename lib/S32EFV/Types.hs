module S32EFV.Types
( Line
, Movement (..)
, Classification (..)
, classificationToFloat
, Aggregate (..)
) where

import           Data.Aeson                 (ToJSON (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           GHC.Generics               (Generic)
import           Text.Megaparsec
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (float)

type Line = Text

data Movement = MkMovement
  { mDate  :: !Text
  , mTipo  :: !Text
  , mMonto :: !Text
  , mRef   :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

data Classification = Wants Movement | Needs Movement deriving Show

classificationToFloat :: Classification -> Maybe Double
classificationToFloat cls =
  let n = mMonto $
        case cls of
          Wants n' -> n'
          Needs n' -> n'
      p :: Parsec Void Text Double = optional (char '$') *> float
   in parseMaybe p (T.replace "," "" n)

instance ToJSON Classification where
  toJSON (Wants m) = toJSON m
  toJSON (Needs m) = toJSON m

data Aggregate = MkAggregate
  { tWants   :: !Double
  , tNeeds   :: !Double
  , tSpent   :: !Double
  , pctWants :: !Double
  , pctNeeds :: !Double
  } deriving Show
