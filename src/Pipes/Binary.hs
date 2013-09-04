{-# LANGUAGE RankNTypes #-}

-- | This module exports facilities that allows you to encode and decode
-- streams of 'Bin.Binary' values. It builds on top of the @pipes@ and
-- @pipes-parse@ package and assumes you understand how to use those libraries.

module Pipes.Binary
  ( -- * Decoding
    -- $decoding
    decode
  , decodeMany
    -- * Encoding
    -- $encoding
  , encode
   -- * Types
  , I.DecodingError(..)
   -- * Exports
   -- $exports
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
import qualified Data.Binary                   as Bin
import           Data.Binary.Get               (ByteOffset)

--------------------------------------------------------------------------------
-- $exports
--
-- The following types are re-exported on this module for your convenience:
--
-- [From "Data.Binary.Get"] 'ByteOffset'

--------------------------------------------------------------------------------
-- $decoding
--
-- There are two different 'Bin.Binary' decoding facilities exported by this
-- module, and choosing between them is easy: If you need to interleave decoding
-- with other stream effects you must use 'decode', otherwise you may use the
-- simpler 'decodeD'.

-- | Tries to decode leading output from the underlying 'Producer' into a
-- 'Bin.Binary' instance, returning either a 'I.DecodingError' on failure, or a
-- pair with the decoded entity together with the number of bytes consumed in
-- order to produce it.
--
-- * /Do not/ use this function if 'Pipes.ByteString.isEndOfBytes' returns
-- 'True', otherwise you may get unexpected decoding errors.
decode
  :: (Monad m, Bin.Binary b)
  => Pp.StateT (Producer B.ByteString m r) m (Either I.DecodingError (ByteOffset, b))
decode = do
    (er, mlo) <- I.parseWith Pp.draw Bin.get
    case mlo of
      Just lo -> Pp.unDraw lo
      Nothing -> return ()
    return er
{-# INLINABLE decode #-}

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
--   'P.errorP' . 'parseMany'
--      :: ('Monad' m, 'Bin.Binary' b)
--      => 'Producer' 'B.ByteString' m r
--      -> 'Producer'' ('ByteOffset', b) ('Control.Monad.Trans.Error.ErrorT' ('I.DecodingError', 'Producer' 'B.ByteString' m r) m) ()
--   @
decodeMany
  :: (Monad m, Bin.Binary b)
  => Producer B.ByteString m r  -- ^Producer from which to draw input.
  -> Producer' (ByteOffset, b) m
               (Either (I.DecodingError, Producer B.ByteString m r) ())
decodeMany src = do
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
            eb <- lift decode
            case eb of
              Left  e -> return (Just e)
              Right b -> yield b >> go
{-# INLINABLE decodeMany #-}

--------------------------------------------------------------------------------

-- | Encodes the given 'Bin.Binary' instance and sends it downstream in
-- 'BS.ByteString' chunks.
encode :: (Monad m, Bin.Binary x) => x -> Producer' B.ByteString m ()
encode = \x -> do
    BLI.foldrChunks (\e a -> respond e >> a) (return ()) (Bin.encode x)
{-# INLINABLE encode #-}

--------------------------------------------------------------------------------
-- XXX: this function is here until pipes-bytestring exports it

-- | Checks if the underlying 'Producer' has any bytes left.
-- Leading 'BS.empty' chunks are discarded.
isEndOfBytes :: Monad m => Pp.StateT (Producer B.ByteString m r) m Bool
isEndOfBytes = do
    ma <- Pp.draw
    case ma of
      Just a
       | B.null a  -> isEndOfBytes
       | otherwise -> Pp.unDraw a >> return False
      Nothing      -> return True
{-# INLINABLE isEndOfBytes #-}

