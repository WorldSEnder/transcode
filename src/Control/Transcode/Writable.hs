module Control.Transcode.Writable
( Writable(..)
, MonadEncodeDiagnostics(..)
, EncodingDiagnostic(..)
) where

import Data.Word (Word8)
import qualified Streaming.ByteString as Q
import qualified Data.ByteString.Builder as QB

-- | Class for byte streams to write to.
-- Most of the interface is modeled to allow an efficient implementation with @ByteStream@
-- but accept other types as well.
class Monad s => Writable s where
  -- | Write a single byte into the stream.
  singleton :: Word8 -> s ()
  writeBuilder :: QB.Builder -> s ()

instance Monad m => Writable (Q.ByteStream m) where
  singleton = Q.singleton
  writeBuilder = Q.fromLazy . QB.toLazyByteString

newtype EncodingDiagnostic = EncodingDiagnostic { reason :: String }

-- | Error reporting facilities of the encoding process.
class MonadEncodeDiagnostics m where
  -- | Report a fatal error and abort the encoding.
  -- Note the polymorphic return type, which is akin to control flow that never returns.
  fatal :: EncodingDiagnostic -> m r
  -- | Report an error in the encoding, but allow encoding to continue.
  error :: EncodingDiagnostic -> m ()
  -- | Report a warning in the encoding process. Allows encoding to continue.
  warn :: EncodingDiagnostic -> m ()
