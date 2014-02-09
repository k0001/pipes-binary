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
{-# LANGUAGE DeriveGeneric      #-}
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
  , module Data.Binary.Put
  , module Data.ByteString
  , module Pipes.Parse

  -- * Tutorial
  -- $tutorial
  ) where

import           Control.Exception                (Exception)
import           Control.Monad.Trans.Error        (Error)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Binary                      (Binary (..))
import qualified Data.Binary
import           Data.Binary.Get                  (ByteOffset, Get)
import qualified Data.Binary.Get                  as Get
import           Data.Binary.Put                  (Put)
import qualified Data.Binary.Put                  as Put
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.Data                        (Data, Typeable)
import           Data.Profunctor                  (Profunctor, dimap)
import           GHC.Generics                     (Generic)
import           Pipes
import qualified Pipes.ByteString
import           Pipes.Parse                      (Parser)

--------------------------------------------------------------------------------

type Iso' a b = forall f p. (Functor f, Profunctor p) => p b (f b) -> p a (f a)

--------------------------------------------------------------------------------

-- | Convert a value to a byte stream.
--
-- Keep in mind that a single encode value might be split into many 'ByteString'
-- chunks, that is, the lenght of the obtained 'Producer' might be greater than
-- 1.
--
-- /Hint:/ You can easily turn this 'Producer'' into a 'Pipe' that encodes
-- 'Binary' instances as they flow downstream using:
--
-- @
-- 'for' 'cat' 'encode' :: ('Monad' m, 'Binary' a) => 'Pipe' a 'B.ByteString' m r
-- @
encode :: (Monad m, Binary a) => a -> Producer' ByteString m ()
encode = encodePut . put
{-# INLINABLE encode #-}
{-# RULES "p >-> for cat encode" forall p .
    p >-> for cat encode = for p (\a -> encodePut (put a))
  #-}

-- | Like 'encode', except this uses an explicit 'Put'.
encodePut :: (Monad m) => Put -> Producer' ByteString m ()
encodePut = Pipes.ByteString.fromLazy . Put.runPut
{-# INLINABLE encodePut #-}
{-# RULES "p >-> for cat encodePut" forall p.
    p >-> for cat encodePut = for p encodePut
  #-}

--------------------------------------------------------------------------------

-- | Parse a value from a byte stream.
decode :: (Monad m, Binary a) => Parser ByteString m (Either DecodingError a)
decode = do
    x <- decodeL
    return (case x of
       Left   e     -> Left  e
       Right (_, a) -> Right a)
{-# INLINABLE decode #-}

-- | An isomorphism between a stream of bytes and a stream of decoded values.
decoded
  :: (Monad m, Binary a)
  => Iso' (Producer ByteString m r)
          (Producer a m (Either (DecodingError, Producer ByteString m r) r))
decoded = dimap _decode (fmap _encode)
  where
    _decode p0 = do
      (mr, p1) <- lift (S.runStateT isEndOfBytes' p0)
      case mr of
         Just r  -> return (Right r)
         Nothing -> do
            (ea, p2) <- lift (S.runStateT decode p1)
            case ea of
               Left  e -> return (Left (e, p2))
               Right a -> yield a >> _decode p2
    _encode p0 = do
      er <- for p0 encode
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
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
  => Iso' (Producer ByteString m r)
          (Producer (ByteOffset, a) m
                    (Either (DecodingError, Producer ByteString m r) r))
decodedL = dimap _decode (fmap _encode)
  where
    _decode p0 = do
      (mr, p1) <- lift (S.runStateT isEndOfBytes' p0)
      case mr of
         Just r  -> return (Right r)
         Nothing -> do
            (ea, p2) <- lift (S.runStateT decodeL p1)
            case ea of
               Left  e -> return (Left (e, p2))
               Right a -> yield a >> _decode p2
    _encode p0 = do
      er <- for p0 (\(_, a) -> encode a)
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
{-# INLINABLE decodedL #-}

--------------------------------------------------------------------------------

-- | Like 'decode', except this requires an explicit 'Get' instead of any
-- 'Binary' instance.
decodeGet :: (Monad m) => Get a -> Parser ByteString m (Either DecodingError a)
decodeGet m = do
    x <- decodeGetL m
    return (case x of
       Left   e     -> Left  e
       Right (_, a) -> Right a)
{-# INLINABLE decodeGet #-}

-- | Like 'decodeL', except this requires an explicit 'Get' instead of any
-- 'Binary' instance.
decodeGetL
  :: (Monad m)
  => Get a -> Parser ByteString m (Either DecodingError (ByteOffset, a))
decodeGetL m = S.StateT (go id (Get.runGetIncremental m))
  where
    go diffP decoder p0 = case decoder of
      Get.Fail _ off str -> return (Left (DecodingError off str), diffP p0)
      Get.Done bs off  a -> return (Right (off, a), yield bs >> p0)
      Get.Partial k      -> do
         x <- next p0
         case x of
            Left   e       -> go diffP (k Nothing) (return e)
            Right (bs, p1) -> go (diffP . (yield bs >>)) (k (Just bs)) p1
{-# INLINABLE decodeGetL #-}

--------------------------------------------------------------------------------

-- | A 'Get' decoding error, as provided by 'Get.Fail'.
data DecodingError = DecodingError
  { deConsumed :: {-# UNPACK #-} !ByteOffset
    -- ^ Number of bytes consumed before the error
  , deMessage  :: !String
    -- ^ Error message
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance Exception DecodingError
instance Error     DecodingError

--------------------------------------------------------------------------------
-- Internal stuff

-- | Like 'Pipes.ByteString.isEndOfBytes', except it returns @'Just' r@ if the
-- there are no more bytes, otherwise 'Nothing'.
isEndOfBytes':: Monad m => S.StateT (Producer ByteString m r) m (Maybe r)
isEndOfBytes' = step =<< S.get
  where
    step p0 = do
      x <- lift (next p0)
      case x of
         Left r       -> S.put (return r) >> return (Just r)
         Right (a,p1)
          | B.null a  -> step p1
          | otherwise -> S.put (yield a >> p1) >> return Nothing
{-# INLINABLE isEndOfBytes' #-}

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
