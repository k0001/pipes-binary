-- | @pipes@ utilities for encoding and decoding values as byte streams
--
-- The tutorial at the bottom of this module illustrates how to use this
-- library.
--
-- In this module, the following type synonym compatible with the @lens@,
-- @lens-family@ and @lens-family-core@ libraries is used but not exported:
--
-- @
-- type Iso' a b = forall f p. ('Functor' f, 'Profunctor' p) => p b (f b) -> p a (f a)
-- @

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}

module Pipes.Binary (
  -- * Encoding
    encode
  -- ** Explicit 'Put'
  , encodePut

  -- * Decoding
  , decode
  , decoded
  -- ** Including lengths
  , decodeL
  , decodedL
  -- ** Explicit 'Get'
  , decodeGet
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

import           Control.Exception         (Exception)
import           Control.Monad.Trans.Error (Error)
import           Data.Binary               (Binary (..))
import qualified Data.Binary
import           Data.Binary.Get           (ByteOffset, Get)
import qualified Data.Binary.Get           as Get
import           Data.Binary.Put           (Put)
import qualified Data.Binary.Put           as Put
import           Data.Data                 (Data, Typeable)
import           Data.Profunctor           (Profunctor, dimap)
import           Pipes
import           Pipes.ByteString          (ByteString)
import qualified Pipes.ByteString
import           Pipes.Parse               (Parser, StateT)
import qualified Pipes.Parse               as PP

--------------------------------------------------------------------------------

type Iso' a b = forall f p. (Functor f, Profunctor p) => p b (f b) -> p a (f a)

--------------------------------------------------------------------------------

-- | Convert a value to a byte stream.
--
-- Keep in mind that a single encode value might be split into many 'ByteString'
-- chunks, that is, the lenght of the obtained 'Producer' might be greater than
-- 1.
encode :: (Monad m, Binary a) => a -> Producer ByteString m ()
encode = encodePut . put
{-# INLINABLE encode #-}

-- | Like 'encode', except this uses an explicit 'Put'
encodePut :: (Monad m) => Put -> Producer ByteString m ()
encodePut = Pipes.ByteString.fromLazy . Put.runPut
{-# INLINABLE encodePut #-}

--------------------------------------------------------------------------------

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
  => Iso' (Producer ByteString m e)
          (Producer a m (DecodingError, Producer ByteString m e))
decoded = dimap to (fmap from)
  where
    to p = do
        (x, p') <- lift (PP.runStateT decode p)
        case x of
            Left  e -> return (e, p')
            Right a -> yield a >> to p'
    from p = do
        (_, p') <- for p encode
        p'
{-# INLINABLE decoded #-}

--------------------------------------------------------------------------------

-- | Like 'decode', but also returns the length of input consumed in order to
-- to decode the value.
decodeL
  :: (Monad m, Binary a)
  => Parser ByteString m (Either DecodingError (ByteOffset, a))
decodeL = decodeGetL get
{-# INLINABLE decodeL #-}

-- | Like 'decoded', except this tags each decoded value with the length of
-- input consumed in order to decode it.
decodedL
  :: (Monad m, Binary a)
  => Iso' (Producer ByteString m e)
          (Producer (ByteOffset, a) m (DecodingError, Producer ByteString m e))
decodedL = dimap to (fmap from)
  where
    to p = do
        (x, p') <- lift (PP.runStateT decodeL p)
        case x of
            Left  e -> return (e, p')
            Right r -> yield r >> to p'
    from p = do
        (_, p') <- for p (\(_, a) -> encode a)
        p'
{-# INLINABLE decodedL #-}

--------------------------------------------------------------------------------

-- | Like 'decode', except this requires an explicit 'Get' instead of any
-- 'Binary' instance.
decodeGet :: (Monad m) => Get a -> Parser ByteString m (Either DecodingError a)
decodeGet m = do
    x <- decodeGetL m
    return $ case x of
        Left   e     -> Left  e
        Right (_, a) -> Right a
{-# INLINABLE decodeGet #-}

-- | Like 'decodeL', except this requires an explicit 'Get' instead of any
-- 'Binary' instance.
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

--------------------------------------------------------------------------------

-- | A 'Get' decoding error, as provided by 'Fail'
data DecodingError = DecodingError
  { deConsumed :: {-# UNPACK #-} !ByteOffset
    -- ^ Number of bytes consumed before the error
  , deMessage  :: !String
    -- ^ Error message
  } deriving (Show, Read, Eq, Data, Typeable)

instance Exception DecodingError
instance Error     DecodingError

--------------------------------------------------------------------------------

-- $exports
--
--  The following types are re-exported from this module for your convenience:
--
--  [From "Data.Binary"] 'Binary'
--
--  [From "Data.Binary.Put"] 'Put'
--
--  [From "Data.Binary.Get"] 'Get', 'ByteOffset'
--
--  [From "Data.ByteString"] 'ByteString'
--
--  [From "Pipes.Parse"] 'Parser'

--------------------------------------------------------------------------------

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
> import Pipes.Parse
> import Prelude hiding (splitAt)
>
> -- We need to import 'zoom', which can be found in many packages and all work
> -- equally fine for our purposes. Read "Pipes.Parse.Tutorial" for details.
> --
> -- * From the package @lens-family-core@: 'Lens.Family.State.Strict.zoom'
> -- * From the package @lens-family@:      'Lens.Family2.State.Strict.zoom'
> -- * From the package @lens@:             'Control.Lens.Zoom.zoom'
> import Lens.Family.State.Strict (zoom)
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
