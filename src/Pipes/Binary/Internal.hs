{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module provides low-level integration with the @binary@ package and is
-- likely to be modified in backwards-incompatible ways in the future.
--
-- Use the "Pipes.Binary" module instead.

module Pipes.Binary.Internal
  ( DecodingError(..)
  , parseWithDraw
  ) where

-------------------------------------------------------------------------------

import           Control.Exception            (Exception)
import           Control.Monad.Trans.Error    (Error)
import qualified Data.ByteString              as B
import qualified Data.Binary.Get              as Get
import           Data.Data                    (Data, Typeable)
import           Pipes                        (Producer)
import qualified Pipes.Parse                  as Pp

-------------------------------------------------------------------------------

-- | A 'Get.Get' decoding error, as provided by 'Get.Fail'.
data DecodingError = DecodingError
  { peConsumed :: Get.ByteOffset -- ^Number of bytes consumed before the error.
  , peMessage  :: String         -- ^Error message.
  } deriving (Show, Read, Eq, Data, Typeable)

instance Exception DecodingError
instance Error     DecodingError

-------------------------------------------------------------------------------

instance Monad m => Error (DecodingError, Producer B.ByteString m r)

-------------------------------------------------------------------------------

-- | Run a 'Get.Get' drawing input from the given monadic action as needed.
parseWith
  :: Monad m
  => m (Maybe B.ByteString)
  -- ^An action that will be executed to provide the parser with more input
  -- as needed. If the action returns 'Nothing', then it's assumed no more
  -- input is available.
  -> Get.Get r
  -- ^Parser to run on the given input.
  -> m (Either DecodingError (Get.ByteOffset, r), Maybe B.ByteString)
  -- ^Either a decoding error or a pair of a result and the number of bytes
  -- consumed, as well as an any leftovers.
parseWith refill = \g -> step (Get.runGetIncremental g)
  where
    step (Get.Partial k)   = refill >>= \a -> step (k a)
    step (Get.Done lo n r) = return (Right (n, r), mayInput lo)
    step (Get.Fail lo n m) = return (Left (DecodingError n m), mayInput lo)
{-# INLINABLE parseWith #-}

-- | Run a parser drawing input from the underlying 'Producer'.
parseWithDraw
  :: Monad m
  => Get.Get b -- ^Parser to run on the given input.
  -> Pp.StateT (Producer B.ByteString m r) m
               (Either DecodingError (Get.ByteOffset, b), Maybe B.ByteString)
parseWithDraw = parseWith $ do
    ea <- Pp.draw
    return (case ea of
      Left  _ -> Nothing
      Right a -> Just a)
{-# INLINABLE parseWithDraw #-}

--------------------------------------------------------------------------------

-- | Wrap @a@ in 'Just' if not-null. Otherwise, 'Nothing'.
mayInput :: B.ByteString -> Maybe B.ByteString
mayInput x | B.null x = Nothing
           | otherwise = Just x
{-# INLINE mayInput #-}
