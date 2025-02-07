module S32EFV.Config where

import           S32EFV.Parse.BancoGuayaquil
import           S32EFV.Parse.Handle

import           Data.Text                   (Text)

wants, needs :: [Text]
wants = ["uber", "sweet", "wellness", "amazon", "mall", "hm", "restaurante", "rappi"]
needs = ["farmacia", "megamaxi"]

parsers :: [ParseHandle]
parsers =
  [ mkBancoGuayaquilParseHandle
  ]
