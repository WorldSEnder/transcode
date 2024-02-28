{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-partial-fields #-} -- we use NoFieldSelectors

module Data.Transcode.Maybe
  ( HasJustValue
  , MaybeCoding
  , codeMaybeDiscrim
  , codeMaybe
  ) where

import           Control.Transcode.Readable as R (DecodingError (..),
                                                  MonadDecodeDiagnostics,
                                                  Readable, fatal, readByte)
import           Control.Transcode.Writable as W (Writable, singleton)
import           Control.Transcoder
import           GHC.TypeLits               (Symbol)

-- Example: en/decoding a Maybe value.

-- ======= NOT YET GENERATED =========
-- This section should be generated derived (with varying bells and whistles) from the datatype definition
-- For now, this is written by hand and has a few pointers for why and what. In general it follows the data type definition

-- For each field in each variant, we define a type alias that will serve as evidence that the field of a value of that variant has a specific value.
-- For Maybe, there is only one such field, which usually has no name, but we will call "value"
type HasJustValue a r = HasVariantField (Maybe a) "Just" "value" r

-- This type will be our evidence obtained from writing the tag for the variant to the band. It has one variant for each variant
-- of the datatype.
data MaybeCoding phase m n a
  -- each case has one field for each field of this variant, plus an additional field used to finish off
  = CaseJust
  { liftValue :: forall r. Transcoder phase a m n r -> Transcoder phase (Maybe a) m n (HasJustValue a r)
  -- ^ each field tells us how to lift a transcoder for that field to a transcoder of the Maybe
  , doneJust :: HasJustValue a (TheUnique a) -> TheUnique (Maybe a)
  -- ^ the additional field tells us how to combine evidence that each of the fields has a specific value to reconstruct the whole Maybe.
  }
  | CaseNothing
  { doneNothing :: TheUnique (Maybe a)
  -- ^ in the Nothing case, the value is already unique and no fields need to be lifted
  }

-- This is used internally to shuttle different data during encoding and decoding. It's the boundary that breaks the "same code for decoding and encoding"
-- doctrine for better peformance.
data StagedData phase (v :: Symbol) a where
  -- During encoding, we shuttle the fields of the value
  StageEncodeJust :: a -> StagedData EncodePhase "Just" a
  StageEncodeNothing :: StagedData EncodePhase "Nothing" a
  -- During decoding, all the additional information needed comes from the evidence, no need for any information
  StageDecode :: StagedData DecodePhase v a

-- The dirty-work method that uses a bunch of "unchecked*" methods to introspect the data and reconstruct evidences.
-- This version commits to a specific encoding for the tag (0x0 for Nothing, 0x1 for Just). This could also be a parameter.
-- Notice that this method does not take as parameter a transcoder for the value: It only encodes the variant of the Maybe.
-- As evidence, it returns a `MaybeCoding` to the user, which reflects the structure and can be used to further encode the value.
codeMaybeDiscrim
  :: forall phase m n a. (Readable n, Writable m, R.MonadDecodeDiagnostics n)
  => Transcoder phase (Maybe a) m n (MaybeCoding phase m n a)
codeMaybeDiscrim = uncheckedTranscode' enc dec
  where
    -- used when the value is some Just, either having read the tag from the band or matching on the input.
    transformJust :: StagedData phase "Just" a -> MaybeCoding phase m n a
    transformJust shuttle = CaseJust
      { liftValue = liftValue
      , doneJust = \(HasVariantField (TheUnique r)) -> uncheckedMkUnique $ Just r
      } where
      -- construct the lifting. Instead of repeatedly matching the input, we take the data from the "shuttle"
      -- during encoding. In any case, we transform the evidence to apply to the specific field of the Maybe
      liftValue :: forall r. Transcoder phase a m n r -> Transcoder phase (Maybe a) m n (HasJustValue a r)
      liftValue valueCode = uncheckedTranscode'
        (\_ -> case shuttle of StageEncodeJust x -> uncheckedMkHasVariantField <$> encodeR valueCode x)
        (uncheckedMkHasVariantField <$> decodeR valueCode)
    -- used when the value is Nothing
    transformNothing :: StagedData phase "Nothing" a -> MaybeCoding phase m n a
    transformNothing _ = CaseNothing
      { doneNothing = uncheckedMkUnique Nothing
      }
    -- encoding primitives: it's trivial to check that the coding is correct from the use of the discriminant
    -- this part is if you will the "critical section" that needs to be checked. It should be easy to see that
    -- this is trivial to generate for ADTs.
    enc :: Encoder phase (Maybe a) m (MaybeCoding phase m n a)
    enc Nothing = do
      W.singleton 0x0
      return $ transformNothing StageEncodeNothing
    enc (Just x) = do
      W.singleton 0x1
      return $ transformJust $ StageEncodeJust x
    dec :: Decoder phase (Maybe a) n (MaybeCoding phase m n a)
    dec = do
      b <- R.readByte
      case b of
        Just 0x0 -> return $ transformNothing StageDecode
        Just 0x1 -> return $ transformJust StageDecode
        Nothing -> R.fatal R.UnexpectedEof
        _ -> R.fatal $ R.UnspecificDecodingError "unexpected discriminant, expected 0x0 or 0x1"
-- ======= NOT YET GENERATED =========

-- Given the above, we can implement a derived Code without having to differentiate between encoding and decoding phase
codeMaybe :: (Readable n, Writable m, R.MonadDecodeDiagnostics n)
          => Code phase m n a -> Code phase m n (Maybe a)
codeMaybe codeValue = do
  codeMaybeDiscrim >>= \case
    CaseJust { liftValue, doneJust } -> do
      x <- liftValue codeValue
      return $ doneJust x
    CaseNothing { doneNothing } -> do
      return doneNothing
