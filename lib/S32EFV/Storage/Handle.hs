module S32EFV.Storage.Handle
( StorageHandle
, saveExpenses
, getExpenses
, mkStorageHandle
)
where

import           S32EFV.Types

data StorageHandle = MkStorageHandle
  { saveExpenses :: [Classification] -> IO ()
  , getExpenses  :: IO [Classification]
  }

mkStorageHandle :: ([Classification] -> IO ()) -> (IO [Classification]) -> StorageHandle
mkStorageHandle = MkStorageHandle
