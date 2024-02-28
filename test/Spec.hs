{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Transcode.Prim
import Control.Transcode.Readable (evalReadableBytes)
import qualified Control.Transcoder as T
import qualified Streaming.ByteString as Q

main :: IO ()
main = do
  -- test decoding
  (30, 42) <- evalReadableBytes (T.decode testCode) "\030\x00\x00\x00\042\x00"
  -- test encoding
  s <- Q.toStrict_ $ T.encode testCode (42, 90)
  True <- pure $ s == "\042\x00\x00\x00\090\000"
  -- postlude
  return ()
  where 
    testCode = word32Host `T.pair` word16Host
