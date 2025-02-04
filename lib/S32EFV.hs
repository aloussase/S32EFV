module S32EFV (classify, aggregate, Movement(..), Classification(..), Aggregate(..)) where

import           Control.Monad ((<=<))
import           Data.List     (foldl')
import           Data.Maybe    (mapMaybe)
import           Data.Text     (Text)
import qualified Data.Text     as T

type Line = Text

data Movement = MkMovement
  { mDate  :: !Text
  , mTipo  :: !Text
  , mMonto :: !Text
  , mRef   :: !Text
  }
  deriving Show

data Classification = Wants Movement | Needs Movement deriving Show

data Aggregate = MkAggregate
  { tWants :: !Double
  , tNeeds :: !Double
  } deriving Show

wants, needs :: [Text]
wants = ["uber", "sweet", "wellness", "amazon", "mall", "hm", "restaurante", "rappi"]
needs = ["farmacia", "megamaxi"]

isExpense :: Movement -> Bool
isExpense (MkMovement _ tipo _ _) = tipo `elem` ["nota debito", "nota de debito"]

-- | Classify the incoming expenses document into 'Wants' and 'Needs'.
classify :: Line -> Maybe Classification
classify = classify' <=< parse . normalize

parse :: Line -> Maybe Movement
parse line | not $ T.null line =
  let parts = T.splitOn "\t" line in
    if length parts < 11 then Nothing
    else pure $ MkMovement
      { mDate = parts !! 1
      , mTipo = parts !! 3
      , mMonto = parts !! 7
      , mRef = parts !! 10
      }
parse _ = Nothing

normalize :: Line -> Line
normalize = T.toLower

classify' :: Movement -> Maybe Classification
classify' movement | isExpense movement = pure $
  if any (\w -> w `T.isInfixOf` (mRef movement)) needs
  then Needs movement
  else if any (\w -> w `T.isInfixOf` (mRef movement)) wants
  then Wants movement
  else Needs movement
classify' _ = Nothing

isWants :: Classification -> Bool
isWants (Wants _) = True
isWants _         = False

toFloat :: Classification -> Maybe Double
toFloat cls =
  let n = mMonto $
        case cls of
          Wants n' -> n'
          Needs n' -> n'
   in read . T.unpack <$> (T.stripPrefix "$" $ T.replace "," "" n)

-- | Aggregate the provided classified expenses.
-- This provides statistics such as total expenses, total wants, percentage expenses; etc.
aggregate :: [Classification] -> Aggregate
aggregate cls = MkAggregate
  (foldl' ((+)) 0 $ mapMaybe toFloat $ filter isWants cls)
  (foldl' ((+)) 0 $ mapMaybe toFloat $ filter (not . isWants) cls)
