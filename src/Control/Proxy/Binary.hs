module Control.Proxy.Binary
  ( -- * Decoding
    decode
  , decodeD
  , encode
  , encodeD
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
-- Requests more input from upstream using 'Pa.draw', when needed.
--
-- This proxy is meant to be composed in the 'P.request' category.
decode
  :: (P.Proxy p, Monad m, Bin.Binary r)
  => ()
  -> P.EitherP ParsingError (P.StateP [BS.ByteString] p)
     () (Maybe BS.ByteString) b' b m r
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
-- This proxy is meant to be composed in the 'P.pull' and 'P.push' categories.
decodeD
  :: (P.Proxy p, Monad m, Bin.Binary b)
  => ()
  -> P.Pipe (P.EitherP ParsingError (P.StateP [BS.ByteString] p))
     (Maybe BS.ByteString) b m ()
decodeD = \() -> loop where
    loop = do
      () <- (decode P.\>\ P.pull) ()
      eof <- P.liftP $ Pa.isEndOfInput
      unless eof loop
{-# INLINABLE decodeD #-}


-- | Encodes the given 'Bin.Binary' instance and sends it downstream in
-- 'BS.ByteString' chunks.
--
-- This proxy is meant to be composed in the 'P.respond' category.
encode
  :: (P.Proxy p, Monad m, Bin.Binary r)
  => r
  -> p x' x () BS.ByteString m ()
encode = \r -> P.runIdentityP $ do
    BLI.foldrChunks (\e a -> P.respond e >> a) (return ()) (Bin.encode r)
{-# INLINABLE encode #-}


-- | Encodes 'Bin.Binary' instances flowing downstream, each in possibly more
-- than one 'BS.ByteString'.
--
-- This proxy is meant to be composed in the 'P.pull' and 'P.push' categories.
encodeD
  :: (P.Proxy p, Monad m, Bin.Binary r)
  => () -> P.Pipe p r BS.ByteString m r
encodeD = P.pull P./>/ encode
{-# INLINABLE encodeD #-}
