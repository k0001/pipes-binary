{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides low-level integration with the @binary@ package and is
-- likely to be modified in backwards-incompatible ways in the future.
--
-- Use the "Control.Proxy.Binary" module instead.

module Control.Proxy.Binary.Internal
  ( ParsingError(..)
  , parseWith
  ) where

-------------------------------------------------------------------------------

import qualified Data.ByteString              as BS
import qualified Data.Binary                  as Bin
import qualified Data.Binary.Get              as Bin
import Control.Exception                      (Exception)
import Data.Data                              (Data, Typeable)

-------------------------------------------------------------------------------

data ParsingError = ParsingError
  { peConsumed :: Bin.ByteOffset -- ^Number of bytes consumed before the error.
  , peMessage  :: String         -- ^Parsing error description message.
  } deriving (Show, Eq, Data, Typeable)

instance Exception ParsingError

-------------------------------------------------------------------------------

-- | Run a parser drawing input from the given monadic action as needed.
parseWith
  :: (Monad m, Bin.Binary r)
  => m (Maybe BS.ByteString)
  -- ^An action that will be executed to provide the parser with more input
  -- as needed. If the action returns 'Nothing', then it's assumed no more
  -- input is available.
  -> Bin.Get r
  -- ^Parser to run on the given input.
  -> m (Either ParsingError r, Maybe BS.ByteString)
  -- ^Either a parser error or a parsed result, together with any leftover.
parseWith refill g = step $ Bin.runGetIncremental g
  where
    step (Bin.Partial k)   = step . k =<< refill
    step (Bin.Done lo _ r) = return (Right r, mayInput lo)
    step (Bin.Fail lo n m) = return (Left (ParsingError n m), mayInput lo)
{-# INLINABLE parseWith #-}

-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: BS.ByteString -> Maybe BS.ByteString
mayInput x | BS.null x = Nothing
           | otherwise = Just x
{-# INLINE mayInput #-}

