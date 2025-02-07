module S32EFV.Parse.Handle (ParseHandle, getIdentifier, parseMovement, mkParseHandle) where

import           S32EFV.Types

import           Data.Text    (Text)

data ParseHandle = MkParseHandle
  { parseMovement :: Text -> Maybe Movement
  , getIdentifier :: Text
  }

mkParseHandle :: Text -> (Text -> Maybe Movement) -> ParseHandle
mkParseHandle gi pm = MkParseHandle pm gi
