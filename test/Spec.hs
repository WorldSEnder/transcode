{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Transcode.Prim
import Data.Transcode.List (codeList)
import Control.Transcode.Readable (evalReadableBytes)
import qualified Control.Transcoder as T
import qualified Streaming.ByteString as Q

main :: IO ()
main = do
  -- TODO: these encodings depend on the platform. E.g. sizeof Int === 8
  -- test decoding
  (30, 42) <- evalReadableBytes (T.decode testCode) "\030\x00\x00\x00\042\x00"
  [42] <- evalReadableBytes (T.decode listCode) "\001\x00\x00\x00\x00\x00\x00\x00\042\x00"
  -- test encoding
  s <- Q.toStrict_ $ T.encode testCode (42, 90)
  True <- pure $ s == "\042\x00\x00\x00\090\x00"

  t <- Q.toStrict_ $ T.encode listCode [42, 90]
  True <- pure $ t == "\002\x00\x00\x00\x00\x00\x00\x00\042\x00\090\x00"
  -- postlude
  return ()
  where 
    testCode = word32Host `T.pair` word16Host
    listCode = codeList word16Host
