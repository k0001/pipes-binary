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
import           Prelude                       hiding (mapM_)

--------------------------------------------------------------------------------

-- | Decodes one 'Bin.Binary' instance flowing downstream.
--
-- In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw' when needed. 'BS.null'
-- inputs from upstream may result in unexpected EOF parsing errors, you can
-- prevent that kind of errors by using the 'skipNullD' proxy upstream.
--
-- This proxy is meant to be composed in the 'P.request' category.

-- In case you wonder, skipping 'BS.null' inputs manually below wouldn't help if
-- we were trying to parse the tail of the stream and there were just 'BS.null'
-- inputs left. That's why in 'decodeD' we just recommend using
-- @P.filterD (not . BS.null)@ upstream, which gives the optimal behavior.
decode
  :: (P.Proxy p, Monad m, Bin.Binary r)
  => ()
  -> P.EitherP ParsingError (P.StateP [BS.ByteString] p)
     () (Maybe BS.ByteString) y' y m r
decode = \() -> do
    (er, mlo) <- P.liftP (I.parseWith Pa.draw Bin.get)
    P.liftP (mapM_ Pa.unDraw mlo)
    either P.throw return er
{-# INLINABLE decode #-}


-- | Decodes 'Bin.Binary' instances flowing downstream until EOF.
--
-- In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
--
-- 'BS.null' input chunks from upstream will cause undesired parsing failures.
-- If you are not sure whether your input stream is free from 'BS.null' chunks,
-- you can use 'P.filterD' upstream:
--
-- > ... >-> P.filterD (not . BS.null) >-> decodeD >-> ...
--
-- This proxy is meant to be composed in the 'P.pull' category.
decodeD
  :: (P.Proxy p, Monad m, Bin.Binary b)
  => ()
  -> P.Pipe (P.EitherP ParsingError (P.StateP [BS.ByteString] p))
     (Maybe BS.ByteString) b m ()
decodeD = \() -> loop where
    loop = do
        eof <- P.liftP Pa.isEndOfInput
        unless eof $ do
          () <- P.respond =<< decode ()
          loop
{-# INLINABLE decodeD #-}


-- | Encodes the given 'Bin.Binary' instance and sends it downstream in
-- 'BS.ByteString' chunks.
--
-- This proxy is meant to be composed in the 'P.respond' category.
encode
  :: (P.Proxy p, Monad m, Bin.Binary r)
  => r -> p x' x () BS.ByteString m ()
encode = \r -> P.runIdentityP $ do
    BLI.foldrChunks (\e a -> P.respond e >> a) (return ()) (Bin.encode r)
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

