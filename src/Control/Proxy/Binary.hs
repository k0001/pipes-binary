module Control.Proxy.Binary
  ( -- * Decoding
    decodeD
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

-- | Decodes binary input flowing downstream until parsing either succeeds or
-- fails. Returns 'Nothing' on EOF.
--
-- In case of parsing errors, a 'ParsingError' exception is thrown in the
-- 'Pe.EitherP' proxy transformer.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
decodeD
  :: (P.Proxy p, Monad m, Bin.Binary r)
  => ()
  -> Pe.EitherP ParsingError (Ps.StateP [BS.ByteString] p)
     () (Maybe BS.ByteString) b' b m (Maybe r)
decodeD = \() -> do
    eof <- P.liftP $ Pa.isEndOfInput
    if eof
      then return Nothing
      else do
        (er, mlo) <- P.liftP $ I.parseWithMay Pa.draw Bin.get
        P.liftP $ mapM_ Pa.unDraw mlo
        case er of
          Left e  -> Pe.throw e
          Right r -> return (Just r)


-- | Try to decode input flowing downstream. Returns 'Just Left' in case of
-- parsing failures and 'Nothing' on EOF.
--
-- Requests more input from upstream using 'Pa.draw', when needed.
eitherDecodeD
  :: (Monad m, P.Proxy p, Bin.Binary r)
  => ()
  -> Ps.StateP [BS.ByteString] p
     () (Maybe BS.ByteString) b' b m (Maybe (Either ParsingError r))
eitherDecodeD = \() -> do
    eof <- Pa.isEndOfInput
    if eof
      then return Nothing
      else do
        (er,mlo) <- I.parseWithMay Pa.draw Bin.get
        mapM_ Pa.unDraw mlo
        return (Just er)

