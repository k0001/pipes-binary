{-# LANGUAGE DeriveDataTypeable #-}

module Pipes.Binary (
    -- * Encoders and Decoders
      encode
    , decode
    , decoded

    -- * Types
    , DecodingError(..)

    -- * Exports
    -- $exports
    , module Data.Binary
    , module Data.Binary.Get
    , module Pipes.Parse
    , module Pipes.ByteString
    ) where

import Control.Exception (Exception)
import Control.Monad.Trans.Error (Error)
import Data.Binary (Binary(..))
import qualified Data.Binary
import Data.Binary.Get (ByteOffset)
import qualified Data.Binary.Get as Get
import Data.Data (Data, Typeable)
import Lens.Family2 (Lens')
import Pipes
import Pipes.ByteString (ByteString)
import qualified Pipes.ByteString
import Pipes.Parse (Parser, StateT)
import qualified Pipes.Parse as PP

-- | A 'Get' decoding error, as provided by 'Fail'
data DecodingError = DecodingError
    { peConsumed :: {-# UNPACK #-} !ByteOffset
      -- ^ Number of bytes consumed before the error
    , peMessage  :: !String
      -- ^ Error message
    } deriving (Show, Read, Eq, Data, Typeable)

instance Exception DecodingError
instance Error     DecodingError

-- | Convert a value to a byte stream
encode :: (Monad m, Binary a) => a -> Producer ByteString m ()
encode a = Pipes.ByteString.fromLazy (Data.Binary.encode a)
{-# INLINABLE encode #-}

-- | Parse a value from a byte stream
decode :: (Monad m, Binary a) => Parser ByteString m (Either DecodingError a)
decode = PP.StateT (go id (Get.runGetIncremental Data.Binary.get))
  where
    go diffP decoder p = case decoder of
        Get.Fail _ off str -> return (Left (DecodingError off str), diffP p)
        Get.Partial k      -> do
            x <- next p
            case x of
                Left   e       -> go diffP (k Nothing) (return e)
                Right (bs, p') -> go (diffP . (yield bs >>)) (k (Just bs)) p'
        Get.Done _ _   a   -> return (Right a, p)
{-# INLINABLE decode #-}

-- | An isomorphism between a stream of bytes and a stream of decoded values
decoded
    :: (Monad m, Binary a)
    => Lens' (Producer ByteString m e)
             (Producer a m (DecodingError, Producer ByteString m e))
decoded k p0 = fmap from (k (to p0))
  where
    to p = do
        (x, p') <- lift (PP.runStateT decode p)
        case x of
            Left  e -> return (e, p')
            Right a -> do
                yield a
                to p'
    from p = do
        (_, p') <- for p encode
        p'
{-# INLINABLE decoded #-}

{- $exports
    The following types are re-exported from this module for your convenience:

    [From "Data.Binary"] 'Binary'

    [From "Data.Binary.Get"] 'ByteOffset'

    [From "Pipes.Parse"] 'Parser'

    [From "Pipes.ByteString"] 'ByteString'
-}
