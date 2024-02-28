{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Transcode.Readable
( Readable(..)
, DecodingError(..)
, MonadDecodeDiagnostics(..)
, ReadableBytes
, evalReadableBytes
) where

import Data.Word (Word8)
import Data.Functor.Of(Of(..))
import qualified Streaming.ByteString as Q
import Control.Monad.Trans.State.Strict (StateT(StateT), evalStateT)
import Control.Monad.Trans.Class
import Debug.Trace
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO, liftIO)

class Monad m => Readable m where
  -- | Read a single byte from the input stream
  readByte :: m (Maybe Word8)
  -- | Split the first @n@ bytes into a strict string and return it.
  -- The returned bytestring might be shorter than @n@, in which case the
  -- end of file has been reached.
  splitStrict :: Int -> m ByteString

newtype ReadableBytes m r = RBStream { stream :: StateT (Q.ByteStream m ()) m r }
  deriving (Functor, Applicative, Monad)

instance MonadTrans ReadableBytes where
  lift m = RBStream { stream = lift m }

evalReadableBytes :: Monad m => ReadableBytes m r -> Q.ByteStream m () -> m r
evalReadableBytes r = evalStateT (stream r)

instance Monad m => Readable (ReadableBytes m) where
  readByte = RBStream {
    stream = StateT $ fmap mapByte . Q.uncons
  } where
    mapByte (Left ~()) = (Nothing, Q.empty)
    mapByte (Right (w, rest)) = (Just w, rest)
  splitStrict n = RBStream {
    stream = StateT $ fmap shuffle . Q.toStrict . Q.splitAt (fromIntegral n)
  } where
    shuffle (string :> rest) = (string, rest)

data DecodingError =
  UnspecificDecodingError String
  | UnexpectedEof
  deriving Show

-- | Error reporting facilities of the decoding process.
class MonadDecodeDiagnostics m where
  -- | Report a fatal error and abort the decoding.
  -- Note the polymorphic return type, which is akin to control flow that never returns.
  fatal :: DecodingError -> m r
  -- | Report an error in the decoding, but allow decoding to continue.
  error :: DecodingError -> m ()
  -- | Report a warning in the decoding process. Allows decoding to continue.
  warn :: DecodingError -> m ()

-- TODO: probably a bad instance. Remove?
instance MonadIO m => MonadDecodeDiagnostics (ReadableBytes m) where
  fatal err = Prelude.error $ show err
  error err = lift $ liftIO $ Debug.Trace.traceIO $ show err
  warn err = lift $ liftIO $ Debug.Trace.traceIO $ show err
