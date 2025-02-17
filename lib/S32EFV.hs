module S32EFV
( classify, aggregate, Movement(..)
, Classification(..), Aggregate(..)
, ParseHandle (getIdentifier, parseMovement)
, storeExpenses, retrieveExpenses
, mkStorageHandle, StorageHandle
, module S32EFV.Config
, module S32EFV.Storage.Postgres
, module S32EFV.Parse.BancoGuayaquil
, module S32EFV.Parse.BancoBolivariano
) where

import           S32EFV.Config
import           S32EFV.Parse.BancoBolivariano
import           S32EFV.Parse.BancoGuayaquil
import           S32EFV.Parse.Handle
import           S32EFV.Storage.Handle
import           S32EFV.Storage.Postgres
import           S32EFV.Types

import           Data.List                     (foldl')
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T

isExpense :: Movement -> Bool
isExpense (MkMovement _ tipo _ _) = tipo `elem` ["nota debito", "nota de debito"]

-- | Classify the incoming expenses document into 'Wants' and 'Needs'.
classify :: ParseHandle -> Line -> Maybe Classification
classify ph line = classify' =<< parseMovement ph (normalize line)

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

-- | Aggregate the provided classified expenses.
-- This provides statistics such as total expenses, total wants, percentage expenses; etc.
aggregate :: [Classification] -> Aggregate
aggregate cls =
  let ws = foldl' ((+)) 0 $ mapMaybe classificationToFloat $ filter isWants cls
      ns = foldl' ((+)) 0 $ mapMaybe classificationToFloat $ filter (not . isWants) cls
      ts = ws + ns
   in MkAggregate (toFixed ws) (toFixed ns) (toFixed ts) (toFixed $ ws / ts) (toFixed $ ns / ts)
  where
    toFixed n = (fromInteger $ round $ n * (10 ^ (2 :: Int))) / (10.0 ^^ (2 :: Int))

type FileContents = Text

-- | Parse expenses from file contents
-- The first parameter tells how many lines to skip before starting to parse.
classifyFromFileContents :: ParseHandle -> Int -> FileContents -> [Classification]
classifyFromFileContents ph n contents =
  let contents' = drop n $ T.splitOn "\n" contents
   in mapMaybe (classify ph) contents'

-- TODO: We might want to expose an API for storing contents from many files at once to avoid multiple DB queries.
storeExpenses :: ParseHandle -> StorageHandle -> Int -> FileContents -> IO ()
storeExpenses ph sh n contents =
  let expenses = classifyFromFileContents ph n contents in
    saveExpenses sh expenses

retrieveExpenses :: StorageHandle -> IO [Classification]
retrieveExpenses sh = getExpenses sh
