module S32EFV.Types where

import           Data.Aeson   (ToJSON (..))
import           Data.Text    (Text)
import           GHC.Generics (Generic)

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
