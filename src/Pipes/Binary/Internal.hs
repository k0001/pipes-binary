{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module provides low-level integration with the @binary@ package and is
-- likely to be modified in backwards-incompatible ways in the future.
--
-- Use the "Pipes.Binary" module instead.

module Pipes.Binary.Internal
  ( DecodingError(..)
  , parseWith
  ) where

-------------------------------------------------------------------------------

import           Control.Exception            (Exception)
import           Control.Monad.Trans.Error    (Error)
import qualified Data.ByteString              as B
import qualified Data.Binary                  as Bin
import qualified Data.Binary.Get              as Bin
import           Data.Data                    (Data, Typeable)
import           Pipes                        (Producer)

-------------------------------------------------------------------------------

data DecodingError = DecodingError
  { peConsumed :: Bin.ByteOffset -- ^Number of bytes consumed before the error.
  , peMessage  :: String         -- ^Error message.
  } deriving (Show, Eq, Data, Typeable)

instance Exception DecodingError
instance Error     DecodingError

-------------------------------------------------------------------------------

instance Monad m => Error (DecodingError, Producer B.ByteString m r)

-------------------------------------------------------------------------------

-- | Run a parser drawing input from the given monadic action as needed.
parseWith
  :: (Monad m, Bin.Binary r)
  => m (Maybe B.ByteString)
  -- ^An action that will be executed to provide the parser with more input
  -- as needed. If the action returns 'Nothing', then it's assumed no more
  -- input is available.
  -> Bin.Get r
  -- ^Parser to run on the given input.
  -> m (Either DecodingError r, Maybe B.ByteString)
  -- ^Either a parser error or a parsed result, together with any leftover.
parseWith refill g = step $ Bin.runGetIncremental g
  where
    step (Bin.Partial k)   = step . k =<< refill
    step (Bin.Done lo _ r) = return (Right r, mayInput lo)
    step (Bin.Fail lo n m) = return (Left (DecodingError n m), mayInput lo)
{-# INLINABLE parseWith #-}

-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: B.ByteString -> Maybe B.ByteString
mayInput x | B.null x = Nothing
           | otherwise = Just x
{-# INLINE mayInput #-}

