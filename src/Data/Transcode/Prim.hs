{-# LANGUAGE ScopedTypeVariables #-}

module Data.Transcode.Prim
( word8Host
, word16Host
, word32Host
) where

import           Control.Transcoder
import           Control.Transcode.Readable            as R
import           Control.Transcode.Writable            as W

import           Prelude                               as P

import qualified Data.ByteString.Builder.Prim          as BP
import qualified Data.ByteString.Builder.Prim.Internal as BPI
import qualified Data.ByteString.Internal              as BI
import           Data.Word                             (Word16, Word32, Word8)
import qualified Foreign                               as F
import           Foreign.Marshal.Utils

import           System.IO.Unsafe                      (unsafeDupablePerformIO)

-- | Encode/decode a storable. The encoding is assumed to be 1-to-1. The size is @sizeOf (undefined :: s)@.
fixedSizeStorable :: forall phase m n s. (Writable m, Readable n, MonadDecodeDiagnostics n, F.Storable s) => Code phase m n s
fixedSizeStorable = uncheckedTranscode enc dec where
  -- to guarantee the laws are not violated, we could reread the s written. We assume this is not necessary
  enc s = uncheckedMkUnique s <$ writeBuilder (BP.primFixed BPI.storableToF s)
  dec = do
    (BI.BS ptr len) <- splitStrict size
    if len < size
    then R.fatal R.UnexpectedEof
    else let s = peekOne ptr
          in P.pure $ uncheckedMkUnique s
  peekOne ptr = unsafeDupablePerformIO $ F.withForeignPtr ptr $ \op ->
    if F.ptrToWordPtr op `mod` fromIntegral alignment == 0
    then F.peek (F.castPtr op)
    else F.alloca $ \tmpOp -> do
      copyBytes tmpOp (F.castPtr op) size
      F.peek tmpOp
  alignment = F.alignment (undefined :: s)
  size = F.sizeOf (undefined :: s)

word8Host :: (Writable m, Readable n, MonadDecodeDiagnostics n) => Code phase m n Word8
word8Host = fixedSizeStorable
word16Host :: (Writable m, Readable n, MonadDecodeDiagnostics n) => Code phase m n Word16
word16Host = fixedSizeStorable
word32Host :: (Writable m, Readable n, MonadDecodeDiagnostics n) => Code phase m n Word32
word32Host = fixedSizeStorable
