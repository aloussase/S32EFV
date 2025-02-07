module S32EFV.Config where

import           S32EFV.Parse.BancoGuayaquil
import           S32EFV.Parse.Handle

parsers :: [ParseHandle]
parsers =
  [ mkBancoGuayaquilParseHandle
  ]
