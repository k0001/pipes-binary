module Control.Proxy.Binary
  ( -- * Decoding
    decodeD
  , maybeDecodeD
  , eitherDecodeD
   -- * Exports
  , module Control.Proxy.Binary.Types
  ) where

-------------------------------------------------------------------------------

import qualified Data.ByteString               as BS
import qualified Control.Proxy                 as P
import           Control.Proxy.Binary.Types
import qualified Control.Proxy.Binary.Internal as I
import qualified Control.Proxy.Parse           as Pa
import qualified Control.Proxy.Trans.Either    as Pe (EitherP, throw)
import qualified Control.Proxy.Trans.State     as Ps (StateP)
import           Data.Binary                   as Bin (Binary(..))
import           Data.Foldable                 (mapM_)
import           Prelude                       hiding (mapM_)

--------------------------------------------------------------------------------

-- | Decodes binary input flowing downstream until decoding either succeeds or
-- fails.
--
-- In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
decodeD
  :: (P.Proxy p, Monad m, Bin.Binary r)
  => ()
  -> Pe.EitherP ParsingError (Ps.StateP [BS.ByteString] p)
     () (Maybe BS.ByteString) b' b m r
decodeD = \() -> do
    (er, mlo) <- P.liftP $ I.parseWithMay Pa.draw Bin.get
    P.liftP $ mapM_ Pa.unDraw mlo
    case er of
      Left e  -> Pe.throw e
      Right r -> return r

-- | Try to decode input flowing downstream, return 'Nothing' in case of
-- parsing failures.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
maybeDecodeD
  :: (Monad m, P.Proxy p, Bin.Binary r)
  => () -> Ps.StateP [BS.ByteString] p () (Maybe BS.ByteString) b' b m (Maybe r)
maybeDecodeD = \() -> do
    (er,mlo) <- I.parseWithMay Pa.draw Bin.get
    mapM_ Pa.unDraw mlo
    case er of
      Left _  -> return Nothing
      Right r -> return (Just r)

-- | Try to decode input flowing downstream, return 'Left' in case of parsing
-- failures.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
eitherDecodeD
  :: (Monad m, P.Proxy p, Bin.Binary r)
  => ()
  -> Ps.StateP [BS.ByteString] p
     () (Maybe BS.ByteString) b' b m (Either ParsingError r)
eitherDecodeD = \() -> do
    (er,mlo) <- I.parseWithMay Pa.draw Bin.get
    mapM_ Pa.unDraw mlo
    return er

