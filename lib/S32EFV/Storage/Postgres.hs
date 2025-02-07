module S32EFV.Storage.Postgres (mkPostgresStorageHandle) where

import           S32EFV.Storage.Handle
import           S32EFV.Types

import           Control.Monad              (void)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B8
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Database.PostgreSQL.Simple
import           System.Environment

mkPostgresStorageHandle :: IO StorageHandle
mkPostgresStorageHandle = do
  connStr <- getEnv "DB_URL"
  conn <- connectPostgreSQL $ B8.pack connStr
  return $ mkStorageHandle (_storeExpenses conn) (_getExpenses conn)
  where
    _getExpenses conn = do
       rs <- query_ conn "select date, amount, tipo_raw, tipo, reference from expenses"
       return $ map tupleToExpense rs

    _storeExpenses conn cls =
      void $ executeMany conn (mconcat
        [ "insert into expenses (date, amount, tipo_raw, tipo, reference) "
        , "values (?, ?, ?, ?, ?)"
        ]) (map expenseToTuple cls)

    tupleToExpense (d, a, tr, "want", r) = Wants $ MkMovement d tr a r
    tupleToExpense (d, a, tr, "need", r) = Needs $ MkMovement d tr a r
    tupleToExpense (_, _, _ , t,      _) = error $ "invalid expense type: " <> t

    expenseToTuple :: Classification -> (ByteString, Double, ByteString, ByteString, ByteString)
    expenseToTuple e@(Wants (MkMovement d tr _ r)) = (TE.encodeUtf8 . toTimestamp $ d, fromJust $ classificationToFloat e, TE.encodeUtf8 tr, "want", TE.encodeUtf8 r)
    expenseToTuple e@(Needs (MkMovement d tr _ r)) = (TE.encodeUtf8 . toTimestamp $ d, fromJust $ classificationToFloat e, TE.encodeUtf8 tr, "need", TE.encodeUtf8 r)

    toTimestamp t =
      let (dd:mm:yyyy:_) = T.splitOn "/" t
       in yyyy <> "-" <> mm <> "-" <> dd
