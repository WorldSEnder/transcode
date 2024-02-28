{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-partial-fields #-} -- we use NoFieldSelectors

module Data.Transcode.List
  ( HasLength
  , codeListLen
  , forEachEv
  , codeList
  ) where
import           Control.Monad              (zipWithM)
import           Control.Transcode.Readable (MonadDecodeDiagnostics, Readable)
import           Control.Transcode.Writable (Writable)
import           Control.Transcoder         (Code, Transcoder, decodeR, encodeR,
                                             getUnique, uncheckedMap,
                                             uncheckedMkUnique,
                                             uncheckedTranscode')
import           Data.Transcode.Prim        (intHost)

newtype HasLength a = HasLength_ Int

codeListLen :: (Writable m, Readable n, MonadDecodeDiagnostics n) => Transcoder phase [a] m n (HasLength a)
codeListLen = HasLength_ . getUnique <$> uncheckedMap length intHost

forEachEv :: (Monad m, Monad n) => HasLength a -> Code phase m n a -> Code phase m n [a]
forEachEv (HasLength_ l) el = uncheckedMkUnique . map getUnique <$> elements where
  elements = sequential $ replicate l el

-- should not be public api. Not proof carrying about `r`!
sequential :: (Monad m, Monad n) => [Transcoder phase a m n r] -> Transcoder phase [a] m n [r]
sequential codes = uncheckedTranscode' (zipWithM ($) (map encodeR codes)) (mapM decodeR codes)

codeList :: (Writable m, Readable n, MonadDecodeDiagnostics n) => Code phase m n a -> Code phase m n [a]
codeList elementCode = do
  evLen <- codeListLen
  forEachEv evLen elementCode
