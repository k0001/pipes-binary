{-# LANGUAGE RankNTypes #-}

-- | This module exports facilities that allows you to encode and decode
-- streams of 'Bin.Binary' values using the @pipes@ and @pipes-parse@ libraries.

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

--------------------------------------------------------------------------------
-- $decoding
--
-- There are two different 'Bin.Binary' decoding facilities exported by this
-- module, and choosing between them is easy: If you need to interleave decoding
-- with other stream effects you must use 'decode', otherwise you may use the
-- simpler 'decodeD'.

-- | Decodes one 'Bin.Binary' instance flowing downstream.
--
-- * In case of decoding errors, a 'I.DecodingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * /Do not/ use this proxy if 'Pipes.ByteString.isEndOfBytes' returns
-- 'True', otherwise you may get unexpected decoding errors.
decode
  :: (Monad m, Bin.Binary b)
  => Pp.StateT (Producer B.ByteString m r) m (Either I.DecodingError b)
decode = do
    (er, mlo) <- I.parseWith Pp.draw Bin.get
    case mlo of
      Just lo -> Pp.unDraw lo
      Nothing -> return ()
    return er
{-# INLINABLE decode #-}


-- | Decodes 'Bin.Binary' instances flowing downstream until end of input.
--
-- * In case of decoding errors, a 'I.DecodingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw', when needed.
--
-- * Empty input chunks flowing downstream will be discarded.
decodeMany
  :: (Monad m, Bin.Binary b)
  => Producer B.ByteString m r  -- ^Producer from which to draw input.
  -> Producer' b m (Either (I.DecodingError, Producer B.ByteString m r) ())
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
-- $encoding
--
-- There are two different 'Bin.Binary' encoding facilities exported by this
-- module, and choosing between them is easy: If you need to interleave encoding
-- with other stream effects you must use 'encode', otherwise you may use the
-- simpler 'encodeD'.

-- | Encodes the given 'Bin.Binary' instance and sends it downstream in
-- 'BS.ByteString' chunks.
encode :: (Monad m, Bin.Binary x) => x -> Producer' B.ByteString m ()
encode = \x -> do
    BLI.foldrChunks (\e a -> respond e >> a) (return ()) (Bin.encode x)
{-# INLINABLE encode #-}

--------------------------------------------------------------------------------
-- XXX: this function is here until pipes-bytestring exports it

-- | Like 'Pa.isEndOfInput', except it also consumes and discards leading
-- empty 'BS.ByteString' chunks.

isEndOfBytes :: Monad m => Pp.StateT (Producer B.ByteString m r) m Bool
isEndOfBytes = do
    ma <- Pp.draw
    case ma of
      Just a
       | B.null a  -> isEndOfBytes
       | otherwise -> Pp.unDraw a >> return False
      Nothing      -> return True
{-# INLINABLE isEndOfBytes #-}

