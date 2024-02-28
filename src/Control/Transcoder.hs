{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RoleAnnotations  #-}

{-# OPTIONS_GHC -Wno-partial-fields #-} -- we use NoFieldSelectors

module Control.Transcoder
  ( Transcoder
  , encodeR
  , decodeR
  , Code
  , encode
  , decode
  , Encoder
  , Decoder
  -- evidence for a specific value
  , TheUnique
  , pattern TheUnique
  , getUnique
  , uncheckedMkUnique
  -- evidence that some field of a structure fulfils some property
  , HasVariantField
  , pattern HasVariantField
  , getVariantField
  , uncheckedMkHasVariantField
  -- various utility
  , mapIso
  -- details the user shouldn't need to use
  , EncodePhase
  , DecodePhase
  , uncheckedTranscode'
  , uncheckedTranscode
  ) where

import qualified Control.Lens.Iso as I
import           Control.Monad    (void)
import           GHC.TypeLits     (Symbol)

-- import Data.Coerce (coerce)

data EncodePhase
data DecodePhase
-- | A transcoder of a type 'a' can store and parse back properties 'r' of the encoded value.
-- A lawful transcoder must guarantee that the *property* returned from encoding is equal to
-- the property returned from decoding the emitted code.
data Transcoder phase a m n r = Transcoder
  { _encode :: (phase ~ EncodePhase) => a -> m r
  , _decode :: (phase ~ DecodePhase) => n r
  }
  deriving Functor

type Encoder phase a m n r = phase ~ EncodePhase => a -> m r
type Decoder phase a m n r = phase ~ DecodePhase => n r
-- | Construct a transcode from an encoder/decoder pair under the assumption of a specific coding phase.
uncheckedTranscode'
  :: Encoder phase a m n r
  -> Decoder phase a m n r
  -> Transcoder phase a m n r
uncheckedTranscode' enc dec = Transcoder { _encode = enc, _decode = dec }

-- | Construct a transcode from an encoder/decoder pair.
uncheckedTranscode
  :: (a -> m r)
  -> n r
  -> Transcoder phase a m n r
uncheckedTranscode enc dec = Transcoder { _encode = enc, _decode = dec }

encodeR :: (phase ~ EncodePhase) => Transcoder phase a m n r -> a -> m r
encodeR Transcoder { _encode } = _encode
decodeR :: (phase ~ DecodePhase) => Transcoder phase a m n r -> n r
decodeR Transcoder { _decode } = _decode

instance (Applicative m, Applicative n) => Applicative (Transcoder phase a m n) where
  -- this code is lawful: it encodes no bytes, and returns the same value during encoding and decoding
  pure r = uncheckedTranscode (\_ -> pure r) (pure r)
    -- this code is lawful if both inputs are lawful
  f <*> x = uncheckedTranscode' (\a -> encodeR f a <*> encodeR x a) (decodeR f <*> decodeR x)

instance (Monad m, Monad n) => Monad (Transcoder phase a m n) where
  return = pure
  m >>= cont = uncheckedTranscode' (\a -> do
        x <- encodeR m a
        encodeR (cont x) a
      ) (do
        x <- decodeR m
        decodeR (cont x)
      )

-- | A special property that signals that the encoded value is equivalent to the contained value
newtype TheUnique a = TheUnique_ a
type role TheUnique representational
-- pattern to match the value in a "unique"
pattern TheUnique :: a -> TheUnique a
pattern TheUnique a <- TheUnique_ a
{-# COMPLETE TheUnique #-}

getUnique :: TheUnique a -> a
getUnique (TheUnique a) = a

-- | Construct a claim that the encoded value is equivalent to the argument.
-- Since the compiler can not verify this claim, the method is named unchecked.
-- Usually, you want to deduce this claim from other properties that have been encoded about a value.
uncheckedMkUnique :: a -> TheUnique a
uncheckedMkUnique = TheUnique_

-- | A code for a type 'a' is a transcoder that deduces that the value is equivalent to a unique value
-- that can also be deduced from the written byte sequence.
type Code phase m n a = Transcoder phase a m n (TheUnique a)
encode :: Functor m => Code EncodePhase m n a -> a -> m ()
encode code a = void $ encodeR code a
decode :: Functor n => Code DecodePhase m n a -> n a
decode code = getUnique <$> decodeR code

mapIso :: (Functor m, Functor n) => I.AnIso a a b b -> Code phase m n a -> Code phase m n b
mapIso iso code = I.withIso iso $ \f g ->
  let coerceEvAB (TheUnique a) = uncheckedMkUnique (f a)
  in uncheckedTranscode' (\b -> coerceEvAB <$> encodeR code (g b)) (coerceEvAB <$> decodeR code)

-- | A claim that the encoded value of type 'a' has the variant 'v', with a field 'f' that has the property
-- claimed by 'r'.
newtype HasVariantField a (v :: Symbol) (f :: Symbol) r = HasVariantField_ r
pattern HasVariantField :: r -> HasVariantField a v f r
pattern HasVariantField a <- HasVariantField_ a
{-# COMPLETE HasVariantField #-}

getVariantField :: HasVariantField a v f r -> r
getVariantField (HasVariantField a) = a
uncheckedMkHasVariantField :: r -> HasVariantField a v f r
uncheckedMkHasVariantField = HasVariantField_
