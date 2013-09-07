{-# LANGUAGE RankNTypes #-}

-- | This module exports facilities that allows you to encode and decode
-- streams of 'Bin.Binary' values. It builds on top of the @pipes@ and
-- @pipes-parse@ package and assumes you understand how to use those libraries.

module Pipes.Binary
  ( -- * @Binary@ instances
    encode
  , decode
  , decodeMany
    -- * @Get@ monad
  , decodeGet
  , decodeGetMany
    -- * @Put@ monad
  , encodePut
    -- * Types
  , I.DecodingError(..)
    -- * Exports
    -- $exports
  , isEndOfBytes
  , module Data.Binary.Get
  ) where

-------------------------------------------------------------------------------

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.Internal as BLI
import           Pipes
import           Pipes.Core
import qualified Pipes.Binary.Internal         as I
import qualified Pipes.Lift                    as P
import qualified Pipes.Parse                   as Pp
import qualified Data.Binary                   as Bin (get, put)
import qualified Data.Binary.Put               as Put (runPut)
--------------------------------------------------------------------------------
import           Data.Binary                   (Binary)
import           Data.Binary.Get               (ByteOffset, Get)
import           Data.Binary.Put               (Put)

-- $exports
--
-- The following types are re-exported on this module for your convenience:
--
-- [From "Data.Binary"] 'Binary'.
--
-- [From "Data.Binary.Get"] 'Get', 'ByteOffset'.
--
-- [From "Data.Binary.Put"] 'Put'.

--------------------------------------------------------------------------------

-- | Try to decode leading output from the underlying 'Producer' into a
-- 'Bin.Binary' instance, returning either a 'I.DecodingError' on failure, or a
-- pair with the decoded entity together with the number of bytes consumed in
-- order to produce it.
--
-- * /Do not/ use this function if 'isEndOfBytes' returns 'True', otherwise you
-- may get unexpected decoding errors.
decode
  :: (Monad m, Binary b)
  => Pp.StateT (Producer B.ByteString m r) m
               (Either I.DecodingError (ByteOffset, b)) -- ^
decode = decodeGet Bin.get
{-# INLINABLE decode #-}

-- | Like 'decode', except it takes an explicit 'Bin.Get' monad.
decodeGet
  :: Monad m
  => Get b  -- ^
  -> Pp.StateT (Producer B.ByteString m r) m
               (Either I.DecodingError (ByteOffset, b))
decodeGet get = do
    (er, mlo) <- I.parseWithDraw get
    case mlo of
      Just lo -> Pp.unDraw lo
      Nothing -> return ()
    return er
{-# INLINABLE decodeGet #-}

-- | Continuously decode output from the given 'Producer' into a 'Bin.Binary'
-- instance, sending downstream pairs of each successfully decoded entity
-- together with the number of bytes consumed in order to produce it.
--
-- This 'Producer' runs until it either runs out of input, in which case it
-- returns @'Right' ()@, or until a decoding failure occurs, in which case
-- it returns a 'Left' providing the 'I.DecodingError' and a 'Producer' with any
-- leftovers.
--
-- Hints:
--
-- * You can use 'P.errorP' to promote the 'Either' return value to an
--   'Control.Monad.Trans.Error.ErrorT' monad transformer, which might be
--   particularly handy if you are trying compose this 'Producer' with another
--   'Proxy' that's not so flexible about the return types it accepts.
--
--   @
--   'P.errorP' . 'decodeMany'
--      :: ('Monad' m, 'Binary' b)
--      => 'Producer' 'B.ByteString' m r
--      -> 'Producer'' ('ByteOffset', b) ('Control.Monad.Trans.Error.ErrorT' ('I.DecodingError', 'Producer' 'B.ByteString' m r) m) ()
--   @
decodeMany
  :: (Monad m, Binary b)
  => Producer B.ByteString m r  -- ^Producer from which to draw input.
  -> Producer' (ByteOffset, b) m
               (Either (I.DecodingError, Producer B.ByteString m r) ())
decodeMany src = decodeGetMany Bin.get src
{-# INLINABLE decodeMany #-}

-- | Like 'decodeMany', except it takes an explicit 'Bin.Get' monad.
decodeGetMany
  :: Monad m
  => Get b
  -> Producer B.ByteString m r  -- ^Producer from which to draw input.
  -> Producer' (ByteOffset, b) m
               (Either (I.DecodingError, Producer B.ByteString m r) ())
decodeGetMany get src = do
    (me, src') <- P.runStateP src go
    return $ case me of
      Just e  -> Left  (e, src')
      Nothing -> Right ()
  where
    go = do
        eof <- lift isEndOfBytes
        if eof
          then return Nothing
          else do
            eb <- lift (decodeGet get)
            case eb of
              Left  e -> return (Just e)
              Right b -> yield b >> go
{-# INLINABLE decodeGetMany #-}

--------------------------------------------------------------------------------

-- | Encodes the given 'Bin.Binary' instance and sends it downstream in
-- 'BS.ByteString' chunks.
encode :: (Monad m, Binary x) => x -> Producer' B.ByteString m ()
encode = \x -> encodePut (Bin.put x)
{-# INLINABLE encode #-}

-- | Like 'encode', except it takes an explicit 'Bin.Put' monad.
encodePut :: Monad m => Put -> Producer' B.ByteString m ()
encodePut = \put -> do
    BLI.foldrChunks (\e a -> respond e >> a) (return ()) (Put.runPut put)
{-# INLINABLE encodePut #-}

--------------------------------------------------------------------------------
-- XXX: this function is here until pipes-bytestring exports it

-- | Checks if the underlying 'Producer' has any bytes left.
-- Leading 'BS.empty' chunks are discarded.
isEndOfBytes :: Monad m => Pp.StateT (Producer B.ByteString m r) m Bool
isEndOfBytes = do
    ma <- Pp.draw
    case ma of
      Left  _      -> return True
      Right a
       | B.null a  -> isEndOfBytes
       | otherwise -> Pp.unDraw a >> return False
{-# INLINABLE isEndOfBytes #-}
{-# DEPRECATED isEndOfBytes
    "Will be removed as soon as the `pipes-bytestring` library exports it" #-}
