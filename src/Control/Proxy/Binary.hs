module Control.Proxy.Binary
  ( -- * Decoding
    decodeD
  , decode
    -- * Encoding
  , encodeD
  , encode
   -- * Exports
  , module Control.Proxy.Binary.Types
  ) where

-------------------------------------------------------------------------------

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Internal as BLI
import           Control.Monad                 (unless)
import qualified Control.Proxy                 as P
import           Control.Proxy.Binary.Types
import qualified Control.Proxy.Binary.Internal as I
import qualified Control.Proxy.Parse           as Pa
import qualified Control.Proxy.Trans.Either    as P
import qualified Control.Proxy.Trans.State     as P
import qualified Data.Binary                   as Bin
import           Data.Foldable                 (mapM_)
import           Data.Function                 (fix)
import           Prelude                       hiding (mapM_)

--------------------------------------------------------------------------------

-- | Decodes one 'Bin.Binary' instance flowing downstream.
--
-- * In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw' when needed.
--
-- * /Do not/ use this proxy if 'Control.Proxy.ByteString.isEndOfBytes' returns
-- 'True', otherwise you may get unexpected parsing errors.
decode
  :: (P.Proxy p, Monad m, Bin.Binary r)
  => P.EitherP ParsingError (P.StateP [BS.ByteString] p)
     () (Maybe BS.ByteString) y' y m r
decode = do
    (er, mlo) <- P.liftP (I.parseWith Pa.draw Bin.get)
    P.liftP (mapM_ Pa.unDraw mlo)
    either P.throw return er
{-# INLINABLE decode #-}


-- | Decodes 'Bin.Binary' instances flowing downstream until EOF.
--
-- * In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- * Requests more input from upstream using 'Pa.draw', when needed.
--
-- * Empty input chunks flowing downstream will be discarded.
decodeD
  :: (P.Proxy p, Monad m, Bin.Binary b)
  => ()
  -> P.Pipe (P.EitherP ParsingError (P.StateP [BS.ByteString] p))
     (Maybe BS.ByteString) b m ()
decodeD = \() -> loop where
    loop = do
        eof <- P.liftP isEndOfBytes
        unless eof $ decode >>= P.respond >> loop
{-# INLINABLE decodeD #-}

--------------------------------------------------------------------------------

-- | Encodes the given 'Bin.Binary' instance and sends it downstream in
-- 'BS.ByteString' chunks.
--
-- This proxy is meant to be composed in the 'P.respond' category.
encode
  :: (P.Proxy p, Monad m, Bin.Binary x)
  => x -> p x' x () BS.ByteString m ()
encode = \x -> P.runIdentityP $ do
    BLI.foldrChunks (\e a -> P.respond e >> a) (return ()) (Bin.encode x)
{-# INLINABLE encode #-}


-- | Encodes 'Bin.Binary' instances flowing downstream, each in possibly more
-- than one 'BS.ByteString'.
--
-- This proxy is meant to be composed in the 'P.pull' category.
encodeD
  :: (P.Proxy p, Monad m, Bin.Binary a)
  => () -> P.Pipe p a BS.ByteString m r
encodeD = P.pull P./>/ encode
{-# INLINABLE encodeD #-}

--------------------------------------------------------------------------------
-- XXX: this function is here until pipes-bytestring exports it

-- | Like 'Pa.isEndOfInput', except it also consumes and discards leading
-- empty 'BS.ByteString' chunks.
isEndOfBytes
  :: (Monad m, P.Proxy p)
  => P.StateP [BS.ByteString] p () (Maybe BS.ByteString) y' y m Bool
isEndOfBytes = fix $ \loop -> do
    ma <- Pa.draw
    case ma of
      Just a
       | BS.null a -> loop
       | otherwise -> Pa.unDraw a >> return False
      Nothing      -> return True
{-# INLINABLE isEndOfBytes #-}

