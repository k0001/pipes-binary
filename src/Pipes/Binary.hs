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
import Data.Binary.Get (ByteOffset)
import qualified Data.Binary.Get as Get
import Data.Data (Data, Typeable)
import Lens.Family2 (Lens')
import Pipes
import Pipes.ByteString (ByteString)
import qualified Pipes.ByteString
import Pipes.Parse (Parser, StateT)
import qualified Pipes.Parse as PP

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
        Get.Done bs _  a   -> return (Right a, yield bs >> p)
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

    [From "Data.Binary.Get"] 'ByteOffset'

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


