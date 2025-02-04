module S32EFV
( classify, aggregate, Movement(..)
, Classification(..), Aggregate(..)
, storeExpenses, retrieveExpenses
, mkStorageHandle, StorageHandle
) where

import           Control.Monad ((<=<))
import           Data.Aeson    (ToJSON (..))
import           Data.List     (foldl')
import           Data.Maybe    (mapMaybe)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           GHC.Generics  (Generic)

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
aggregate cls =
  let ws = foldl' ((+)) 0 $ mapMaybe toFloat $ filter isWants cls
      ns = foldl' ((+)) 0 $ mapMaybe toFloat $ filter (not . isWants) cls
      ts = ws + ns
   in MkAggregate (toFixed ws) (toFixed ns) (toFixed ts) (toFixed $ ws / ts) (toFixed $ ns / ts)
  where
    toFixed n = (fromInteger $ round $ n * (10 ^ (2 :: Int))) / (10.0 ^^ (2 :: Int))

type FileContents = Text

-- | Parse expenses from file contents
-- The first parameter tells how many lines to skip before starting to parse.
classifyFromFileContents :: Int -> FileContents -> [Classification]
classifyFromFileContents n contents =
  let contents' = drop n $ T.splitOn "\n" contents
   in mapMaybe classify contents'

-- * Storage stuff

data StorageHandle = MkStorageHandle
  { saveExpenses :: [Classification] -> IO ()
  , getExpenses  :: IO [Classification]
  }

mkStorageHandle :: ([Classification] -> IO ()) -> (IO [Classification]) -> StorageHandle
mkStorageHandle = MkStorageHandle

-- TODO: We might want to expose an API for storing contents from many files at once to avoid multiple DB queries.
storeExpenses :: StorageHandle -> Int -> FileContents -> IO ()
storeExpenses sh n contents =
  let expenses = classifyFromFileContents n contents in
    saveExpenses sh expenses

retrieveExpenses :: StorageHandle -> IO [Classification]
retrieveExpenses sh = getExpenses sh
