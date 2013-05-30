{-# LANGUAGE DeriveDataTypeable #-}

module Control.Proxy.Binary.Types
  ( ParsingError(..)
  ) where

-------------------------------------------------------------------------------

import Control.Exception            (Exception)
import Data.Binary.Get              (ByteOffset)
import Data.Data                    (Data, Typeable)

-------------------------------------------------------------------------------

data ParsingError = ParsingError
  { peConsumed :: ByteOffset -- ^Number of bytes consumed before the error.
  , peMessage  :: String     -- ^ Parsing error description message.
  } deriving (Show, Eq, Data, Typeable)

instance Exception ParsingError

