{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}

import Data.Transcode.Prim
import qualified Control.Transcode as T
import qualified Streaming.ByteString as Q

{-
data Tag a where
  AString :: Tag String
  AnInt :: Tag Int

codeTag :: Monad m => T.Transcode m (Tag a) ((forall x. Tag x -> r) -> r) ()
codeTag = void $ T.uncheckedBimap tagToByte byteToTag T.word8 where
  byteToTag :: Word8 -> (forall x. Tag x -> r) -> r
  byteToTag = \case 0 -> (\f -> f AString) ;
                    1 -> (\f -> f AnInt) ;
                    _ -> error "No matching tag"
  tagToByte :: Tag x -> Word8
  tagToByte = \case AString -> 0 ;
                    AnInt -> 1
-}

main :: IO ()
main = do
  -- test decoding
  (30, 42) <- T.dec testCode "\030\x00\x00\x00\042\x00"
  -- test encoding
  s <- Q.toStrict_ $ T.enc testCode (42, 90)
  True <- pure $ s == "\042\x00\x00\x00\090\000"
  -- postlude
  return ()
  where testCode = word32Host `T.pair` word16Host
