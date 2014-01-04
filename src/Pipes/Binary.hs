{-| @pipes@ utilities for encoding and decoding values as byte streams

    The tutorial at the bottom of this module illustrates how to use this
    library.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Pipes.Binary (
    -- * Encoders and Decoders
      encode
    , decode
    , decoded

    -- * Byte Offsets
    , decodeL
    , decodedL

    -- * Get and Put
    , encodePut
    , decodeGetL

    -- * Types
    , DecodingError(..)

    -- * Exports
    -- $exports
    , module Data.Binary
    , module Data.Binary.Get
    , module Pipes.Parse
    , module Pipes.ByteString

    -- * Tutorial
    -- $tutorial
    ) where

import Control.Exception (Exception)
import Control.Monad.Trans.Error (Error)
import Data.Binary (Binary(..))
import qualified Data.Binary
import Data.Binary.Get (ByteOffset, Get)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import Data.Data (Data, Typeable)
import Lens.Family2 (Lens')
import Pipes
import Pipes.ByteString (ByteString)
import qualified Pipes.ByteString
import Pipes.Parse (Parser, StateT)
import qualified Pipes.Parse as PP

-- | Convert a value to a byte stream
encode :: (Monad m, Binary a) => a -> Producer ByteString m ()
encode a = encodePut (put a)
{-# INLINABLE encode #-}

-- | Parse a value from a byte stream
decode :: (Monad m, Binary a) => Parser ByteString m (Either DecodingError a)
decode = do
    x <- decodeL
    return $ case x of
        Left   e     -> Left  e
        Right (_, a) -> Right a
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

{-| Like 'decode', but also returns the length of input consumed to decode the
    value
-}
decodeL
    :: (Monad m, Binary a)
    => Parser ByteString m (Either DecodingError (ByteOffset, a))
decodeL = decodeGetL get
{-# INLINABLE decodeL #-}

{-| Like 'decoded', except this tags each decoded value with the length of input
    consumed to decode the value
-}
decodedL
    :: (Monad m, Binary a)
    => Lens'
        (Producer ByteString m e)
        (Producer (ByteOffset, a) m (DecodingError, Producer ByteString m e))
decodedL k p0 = fmap from (k (to p0))
  where
    to p = do
        (x, p') <- lift (PP.runStateT decodeL p)
        case x of
            Left  e -> return (e, p')
            Right r -> do
                yield r
                to p'
    from p = do
        (_, p') <- for p (\(_, a) -> encode a)
        p'
{-# INLINABLE decodedL #-}

-- | Like 'encode', except this uses an explicit 'Put'
encodePut :: (Monad m) => Put -> Producer ByteString m ()
encodePut m = Pipes.ByteString.fromLazy (Put.runPut m)
{-# INLINABLE encodePut #-}

-- | Like 'decodeL', except this uses an explicit 'Get'
decodeGetL
    :: (Monad m)
    => Get a -> Parser ByteString m (Either DecodingError (ByteOffset, a))
decodeGetL m = PP.StateT (go id (Get.runGetIncremental m))
  where
    go diffP decoder p = case decoder of
        Get.Fail _ off str -> return (Left (DecodingError off str), diffP p)
        Get.Partial k      -> do
            x <- next p
            case x of
                Left   e       -> go diffP (k Nothing) (return e)
                Right (bs, p') -> go (diffP . (yield bs >>)) (k (Just bs)) p'
        Get.Done bs off  a -> return (Right (off, a), yield bs >> p)
{-# INLINABLE decodeGetL #-}

-- | A 'Get' decoding error, as provided by 'Fail'
data DecodingError = DecodingError
    { peConsumed :: {-# UNPACK #-} !ByteOffset
      -- ^ Number of bytes consumed before the error
    , peMessage  :: !String
      -- ^ Error message
    } deriving (Show, Read, Eq, Data, Typeable)

instance Exception DecodingError
instance Error     DecodingError

{- $exports
    The following types are re-exported from this module for your convenience:

    [From "Data.Binary"] 'Binary'

    [From "Data.Binary.Put"] 'Put'

    [From "Data.Binary.Get"] 'Get', 'ByteOffset'

    [From "Pipes.Parse"] 'Parser'

    [From "Pipes.ByteString"] 'ByteString'
-}

{- $tutorial

    Use 'encode' to convert values to byte streams

> -- example.hs
>
> import Pipes
> import qualified Pipes.Prelude as P
> import Pipes.Binary
>
> readInts :: Int -> Producer Int IO ()
> readInts n = P.readLn >-> P.take n
>
> encodedValues :: Producer ByteString IO ()
> encodedValues = do
>     for (readInts 3) encode  -- Encode 3 Ints read from user input
>     encode 'C'               -- Encode a 'Char'
>     encode True              -- Encode a 'Bool'

    Use 'decode' to parse a single decoded value or 'decoded' to access a stream
    of decoded values:

> -- example.hs
>
> import Data.ByteString (ByteString)
> import Lens.Family.State.Strict (zoom)
> import Pipes.Parse
> import Prelude hiding (splitAt)
>
> decoder :: Parser ByteString IO ()
> decoder = do
>     xs <- zoom (decoded . splitAt 3) drawAll      -- Decode up to three 'Int's
>     lift $ print (xs :: [Int])
>     y  <- decode                                  -- Decode a single 'Char'
>     lift $ print (y :: Either DecodingError Char)
>     z  <- zoom decoded draw                       -- Same as 'decode', but
>     lift $ print (z :: Maybe Bool)                -- with a 'Maybe'
>
> main = evalStateT decoder encodedValues

    Here are some example inputs:

> $ ./example
> 1<Enter>
> 2<Enter>
> 3<Enter>
> [1,2,3]
> Right 'C'
> Just True
> $ ./example
> <Ctrl-D>
> []
> Right 'C'
> Just True

-}
