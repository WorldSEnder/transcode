{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Transcode
  ( Transcode
  , enc
  , enc'
  , dec
  , dec'
  , pair
  , seqPair
  , uncheckedBimap
  , transcoder
  , mapIso
  ) where

import qualified Streaming.ByteString as Q
import qualified Control.Lens.Iso as I
import Prelude as P
import Data.Functor (($>))
import Control.Transcode.Readable (ReadableBytes, evalReadableBytes)
--import Data.Kind (Type)

data DecodeResult d r = DeRes d !r
pattern ReDes r d = DeRes d r

mapRes :: (r -> r') -> DecodeResult d r -> DecodeResult d r'
mapRes fr ~(DeRes d r) = DeRes d (fr r)

mapDecRes :: (d -> d') -> DecodeResult d r -> DecodeResult d' r
mapDecRes fd (DeRes d r) = DeRes (fd d) r

data Transcode m n e d r = Transcode
  { encode :: e -> m r
  , decode :: n (DecodeResult d r)
  }

enc :: Monad m => Transcode (Q.ByteStream m) (ReadableBytes m) e d r -> e -> Q.ByteStream m ()
enc = enc'
enc' :: Monad m => Transcode m n e d r -> e -> m ()
enc' code e = encode code e $> ()

dec :: Monad m => Transcode (Q.ByteStream m) (ReadableBytes m) e d r -> Q.ByteStream m () -> m d
dec code = evalReadableBytes (dec' code)
dec' :: Monad n => Transcode m n e d r -> n d
dec' code = unwrap <$> decode code where
  unwrap (DeRes d _) = d

-- | Low-level primitive to construct a potentially unlawful transcoder
transcoder :: (Monad m, Monad n) => (e -> m r) -> n (d, r) -> Transcode m n e d r
transcoder thisEnc thisDec = Transcode
  { encode = thisEnc
  , decode = wrap <$> thisDec
  } where
  wrap (d, r) = DeRes d r

pure :: (Monad m, Monad n) => a -> Transcode m n () () a
pure a = Transcode
  { encode = \_ -> P.pure a
  , decode = P.pure $ DeRes () a
  }

instance (Monad m, Monad n) => Functor (Transcode m n e d) where
  fmap f code = Transcode
    { encode = fmap f . encode code
    , decode = mapRes f <$> decode code
    }

instance (d ~ (), Monad m, Monad n) => Applicative (Transcode m n e d) where
  pure = Control.Transcode.first (const ()) . Control.Transcode.pure
  l <*> r = Transcode
    { encode = \e -> do
        f <- encode l e
        a <- encode r e
        P.pure $ f a
    , decode = do
        DeRes ~() f <- decode l
        DeRes ~() a <- decode r
        P.pure $ DeRes () (f a)
    }

seqPair :: (Monad m, Monad n) => Transcode m n e c x -> (x -> Transcode m n f d y) -> Transcode m n (e, f) (c, d) y
fstCode `seqPair` sndCode = Transcode
  { encode = \(e, f) -> do
      x <- encode fstCode e
      encode (sndCode x) f
  , decode = do
      DeRes c x <- decode fstCode
      DeRes d y <- decode (sndCode x)
      P.pure $ ReDes y (c, d)
  }

pair :: (Monad m, Monad n) => Transcode m n e c x -> Transcode m n f d y -> Transcode m n (e, f) (c, d) y
pair fstCode sndCode = fstCode `seqPair` const sndCode

uncheckedBimap :: (Monad m, Monad n) => (e -> e') -> (d' -> d) -> Transcode m n e' d' a -> Transcode m n e d a
uncheckedBimap fe fd code = Transcode
  { encode = encode code . fe
  , decode = mapDecRes fd <$> decode code
  }

first :: (e -> e') -> Transcode m n e' d a -> Transcode m n e d a
first fe code = Transcode
  { encode = encode code . fe
  , decode = decode code
  }

mapIso :: (Monad m, Monad n) => I.AnIso s t a b -> Transcode m n t s x -> Transcode m n b a x
mapIso i code = I.withIso i $ \to from -> uncheckedBimap from to code

-- | An abstract existential type @x == ∃(a::k). ExFamily x a@
--class Existential x k | x -> k where
--  type ExFamily x (a :: k) :: Type
--  hide :: forall (a :: k) proxy. proxy a -> ExFamily x a -> x
--  match :: (forall (a :: k). proxy a -> ExFamily x a -> r) -> x -> r

funPair :: (Monad m, Monad n) => Transcode m n fe fd x -> (x -> Transcode m n fe (fd -> sd) y) -> Transcode m n fe sd y
funPair fstCode sndCode = Transcode
  { encode = \fe -> do
      x <- encode fstCode fe
      encode (sndCode x) fe
  , decode = do
      DeRes arg x <- decode fstCode
      DeRes contf y <- decode (sndCode x)
      P.pure $ ReDes y $ contf arg
  }
