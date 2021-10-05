{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language DeriveAnyClass #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Juice
  ( CompressedValues(..)
  , CompressedValue(..)
  , Value(..)
  , decompress
  , decompressMany
  , compress
  , compressMany
  , encode
  , encodeMany
  , decode
  , decodeMany
  , parser
  ) where

import Prelude hiding (True,False)

import Control.Monad.ST (runST)
import Control.Applicative (liftA2)
import Control.Exception (Exception,throwIO)
import Data.Set (Set)
import Data.Functor.Classes (liftEq)
import Data.Primitive (SmallArray,ByteArray(..))
import Data.Foldable (foldl',foldlM)
import GHC.IO (unsafeIOToST)
import Arithmetic.Types (Fin(Fin),type (:=:),Nat)
import Arithmetic.Nat ((<?))
import Data.Number.Scientific (Scientific)
import Data.Word (Word32)
import Data.Bytes (Bytes)
import Data.Kind (Type)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Builder (Builder)
import Data.Text.Short.Unlifted (ShortText#)
import Data.Text.Short (ShortText)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Map.Strict (Map)
import Vector.Unlifted (Length(Known,Unknown))
import Arithmetic.Unsafe(Fin#)
import GHC.Exts (Int(I#))

import qualified Arithmetic.Types as Arithmetic
import qualified Arithmetic.Unsafe as Unsafe
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Nat as Nat
import qualified Data.Set as Set
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Leb128 as Leb128
import qualified Data.Map.Strict as Map
import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as Contiguous
import qualified Json
import qualified Prelude
import qualified Vector.Unlifted as Unlifted
import qualified Vector.Unlifted as UVec
import qualified Data.Text.Short.Unlifted as TS
import qualified Data.Text.Short as TS
import qualified GHC.TypeNats as GHC

data Dictionaries :: GHC.Nat -> GHC.Nat -> Type where
  Dictionaries ::
       !(Unlifted.Vector ('Known k) ShortText#)
    -> !(Unlifted.Vector ('Known s) ShortText#)
    -> Dictionaries k s

data CompressedValues :: Type where
  CompressedValues :: forall (k :: GHC.Nat) (s :: GHC.Nat).
       {-# UNPACK #-} !(Arithmetic.Nat k)
    -> {-# UNPACK #-} !(Arithmetic.Nat s)
    -> {-# UNPACK #-} !(Dictionaries k s)
    -> {-# UNPACK #-} !(SmallArray (Value k s))
    -> CompressedValues

data CompressedValue :: Type where
  CompressedValue :: forall (k :: GHC.Nat) (s :: GHC.Nat).
       {-# UNPACK #-} !(Arithmetic.Nat k)
    -> {-# UNPACK #-} !(Arithmetic.Nat s)
    -> {-# UNPACK #-} !(Dictionaries k s)
    -> !(Value k s)
    -> CompressedValue

instance Eq CompressedValue where
  CompressedValue k1 s1 d1 v1 == CompressedValue k2 s2 d2 v2
    | Just eqK <- Nat.testEqual k1 k2
    , Just eqS <- Nat.testEqual s1 s2
    , eqDict k2 s2 (substituteDict eqK eqS d1) d2
    , eqValue v1 v2 = Prelude.True
    | otherwise = Prelude.False

substituteDict ::
     k1 :=: k2
  -> s1 :=: s2
  -> Dictionaries k1 s1
  -> Dictionaries k2 s2
substituteDict !eqK !eqS (Dictionaries k s) =
  Dictionaries (Unlifted.substitute eqK k) (Unlifted.substitute eqS s) 

eqDict ::
     Nat k
  -> Nat s
  -> Dictionaries k s
  -> Dictionaries k s
  -> Prelude.Bool
eqDict !k !s (Dictionaries x1 y1) (Dictionaries x2 y2) =
  unliftedEq k x1 x2
  &&
  unliftedEq s y1 y2

unliftedEq ::
     Nat n
  -> Unlifted.Vector ('Known n) ShortText#
  -> Unlifted.Vector ('Known n) ShortText#
  -> Prelude.Bool
unliftedEq !n !xs !ys = Fin.descend n Prelude.True
  (\ix r -> TS.lift (Unlifted.index xs ix) == TS.lift (Unlifted.index ys ix) && r)

data Value k s
  = Object !(SmallArray (Member k s))
  | Array !(SmallArray (Value k s))
  | String !(Unsafe.Fin# s)
  | Number {-# UNPACK #-} !Scientific
  | Null
  | True
  | False

eqValue ::
     Value k1 s1
  -> Value k2 s2
  -> Prelude.Bool
eqValue Null Null = Prelude.True
eqValue True True = Prelude.True
eqValue False False = Prelude.True
eqValue (Number a) (Number b) = a == b
eqValue (String (Unsafe.Fin# a)) (String (Unsafe.Fin# b)) = I# a == I# b
eqValue (Array a) (Array b) = liftEq eqValue a b
eqValue (Object a) (Object b) = liftEq eqMember a b
eqValue _ _ = Prelude.False

instance Eq (Value k s) where
  (==) = eqValue

data Member k s = Member
  { key :: !(Fin# k)
  , value :: !(Value k s)
  }

instance Eq (Member k s) where
  (==) = eqMember

eqMember :: Member k1 s1 -> Member k2 s2 -> Bool
eqMember (Member (Unsafe.Fin# k1) v1) (Member (Unsafe.Fin# k2) v2) =
  (I# k1 == I# k2) && eqValue v1 v2

encodeMany :: CompressedValues -> Builder
encodeMany (CompressedValues k s dict vals) =
  encodeNat k <>
  encodeNat s <>
  encodeDictionaries k s dict <>
  encodeSmallArray encodeValue vals

encode :: CompressedValue -> Builder
encode (CompressedValue k s dict val) =
  encodeNat k <>
  encodeNat s <>
  encodeDictionaries k s dict <>
  encodeValue val

decode :: Bytes -> Maybe CompressedValue
decode = Parser.parseBytesMaybe (parserOne <* Parser.endOfInput ())

decodeMany :: Bytes -> Maybe CompressedValues
decodeMany = Parser.parseBytesMaybe (parser <* Parser.endOfInput ())

-- Invariant: result is non-negative.
parseInt :: Parser () s Int
parseInt = do
  n <- Leb128.word32 ()
  -- TODO: This can create negative lengths on 32-bit platforms.
  pure (fromIntegral @Word32 @Int n)

parser :: Parser () s CompressedValues
parser = do
  k <- parseInt
  s <- parseInt
  -- TODO: This can create negative lengths on 32-bit platforms.
  Nat.with k $ \k' ->
    Nat.with s $ \s' -> do
      dict <- parseDictionaries k' s'
      vals <- parseValues k' s'
      pure (CompressedValues k' s' dict vals)

parserOne :: Parser () s CompressedValue
parserOne = do
  k <- parseInt
  s <- parseInt
  -- TODO: This can create negative lengths on 32-bit platforms.
  Nat.with k $ \k' ->
    Nat.with s $ \s' -> do
      dict <- parseDictionaries k' s'
      val <- parseValue k' s'
      pure (CompressedValue k' s' dict val)

parseDictionaries :: Nat k -> Nat str -> Parser () s (Dictionaries k str)
parseDictionaries k s = liftA2 Dictionaries
  (parseSizedTexts k)
  (parseSizedTexts s)

parseSizedTexts :: Nat n -> Parser () s (Unlifted.Vector ('Known n) ShortText#)
parseSizedTexts !n = do
  m <- Parser.effect (Unlifted.initialized n (TS.unlift mempty))
  Fin.ascendM_ n $ \ix -> do
    t <- parseShortText
    Parser.effect (Unlifted.write m ix (TS.unlift t))
  Parser.effect (Unlifted.unsafeFreeze m)

parseValues :: Nat k -> Nat str -> Parser () s (SmallArray (Value k str))
parseValues !k !s = do
  n <- parseInt
  Parser.replicate n (parseValue k s)

parseValue :: Nat k -> Nat str -> Parser () s (Value k str)
parseValue !k !s = Parser.any () >>= \case
  0 -> do
    n <- parseInt
    xs <- Parser.replicate n (parseMember k s)
    pure $! Object xs
  1 -> do
    n <- parseInt
    xs <- Parser.replicate n (parseValue k s)
    pure $! Array xs
  2 -> do
    ix <- parseInt
    Nat.with ix $ \ix' -> case ix' <? s of
      Just lt -> pure $! String (Fin.unlift (Fin ix' lt))
      Nothing -> Parser.fail ()
  3 -> do
    -- TODO: This is broken on 32-bit platforms.
    coeff <- Leb128.int64 ()
    e <- Leb128.int64 ()
    pure $! Number $! SCI.small (fromIntegral coeff) (fromIntegral e)
  5 -> pure Null
  6 -> pure True
  7 -> pure False
  _ -> Parser.fail ()

parseMember :: Nat k -> Nat str -> Parser () s (Member k str)
parseMember !k !s = do
  ix <- parseInt
  Nat.with ix $ \keyIx -> case keyIx <? k of
    Just keyLt -> do
      value <- parseValue k s
      pure $! Member{key=Fin.unlift (Fin keyIx keyLt),value}
    Nothing -> Parser.fail ()

encodeNat :: Arithmetic.Nat n -> Builder
{-# inline encodeNat #-}
encodeNat = encodePosInt . Nat.demote

encodeFin# :: Unsafe.Fin# n -> Builder
{-# inline encodeFin# #-}
encodeFin# (Unsafe.Fin# i) = encodePosInt (I# i)

encodePosInt :: Int -> Builder
encodePosInt = Builder.wordLEB128 . fromIntegral @Int @Word

encodeSmallArray :: (a -> Builder) -> SmallArray a -> Builder
{-# inline encodeSmallArray #-}
encodeSmallArray f !xs =
  encodePosInt (PM.sizeofSmallArray xs) <>
  foldr (\val rest -> f val <> rest) mempty xs

encodeMember :: Member k s -> Builder
encodeMember Member{key,value} =
  encodeFin# key <> encodeValue value

encodeDictionaries ::
     Arithmetic.Nat k
  -> Arithmetic.Nat s
  -> Dictionaries k s
  -> Builder
encodeDictionaries !k !s (Dictionaries ks ss) =
  Unlifted.foldrMap encodeShortText# (Unlifted.slice k ks)
  <>
  Unlifted.foldrMap encodeShortText# (Unlifted.slice s ss)

-- First, we encode the length with LEB-128, then we push
-- the bytes directly (UTF-8 encoded).
encodeShortText# :: ShortText# -> Builder
encodeShortText# (TS.ShortText# b# ) =
  encodePosInt (PM.sizeofByteArray b)
  <>
  Builder.copy (Bytes.fromByteArray b)
  where
  b = ByteArray b#

parseShortText :: Parser () s ShortText
parseShortText = do
  len <- parseInt
  s <- Parser.take () len
  case Bytes.toByteArrayClone s of
    ByteArray x -> case TS.fromShortByteString (SBS x) of
      Nothing -> Parser.fail ()
      Just y -> pure y

encodeValue :: Value k s -> Builder
encodeValue = \case
  Object ms -> Builder.word8 0 <> encodeSmallArray encodeMember ms
  Array vs -> Builder.word8 1 <> encodeSmallArray encodeValue vs
  String ix -> Builder.word8 2 <> encodeFin# ix
  Number sci -> SCI.withExposed
    (\coeff e -> Builder.word8 3 <> Builder.intLEB128 coeff <> Builder.intLEB128 e)
    (\_ _ -> error "encodeValue: write this part")
    -- (\coeff e -> Builder.word8 4 <> Builder.integerLEB128 coeff <> Builder.integerLEB128 e)
    sci
  Null -> Builder.word8 5
  True -> Builder.word8 6
  False -> Builder.word8 7

decompressOne ::
     Arithmetic.Nat k
  -> Arithmetic.Nat s
  -> Dictionaries k s
  -> Value k s
  -> Json.Value
decompressOne !k !s (Dictionaries ks ss) val = case val of
  Null -> Json.Null
  True -> Json.True
  False -> Json.False
  Number sci -> Json.Number sci
  String fin -> Json.String (TS.lift (Unlifted.index# (Unlifted.unlift ss) fin))
  Array cs -> Json.Array $! Contiguous.map'
    (\v -> decompressOne k s (Dictionaries ks ss) v
    ) cs
  Object cs -> Json.Object $! Contiguous.map'
    (\Member{key,value} -> Json.Member
      { key = TS.lift (Unlifted.index# (Unlifted.unlift ks) key)
      , value = decompressOne k s (Dictionaries ks ss) value
      }
    ) cs

decompress :: CompressedValue -> Json.Value
decompress (CompressedValue k s dict val) =
  decompressOne k s dict val

decompressMany :: CompressedValues -> SmallArray Json.Value
decompressMany (CompressedValues k s dict vals) = Contiguous.map'
  (\val -> decompressOne k s dict val
  ) vals

-- Precondition: all strings in the values must appear in the dictionaries.
applyDictionaries ::
     Map ShortText (Fin k)
  -> Map ShortText (Fin str)
  -> Json.Value
  -> Value k str
applyDictionaries ks strs = \case
  Json.Null -> Null
  Json.True -> True
  Json.False -> False
  Json.Number n -> Number n
  Json.Array xs -> Array (Contiguous.map' (applyDictionaries ks strs) xs)
  Json.String str -> case Map.lookup str strs of
    Nothing -> errorWithoutStackTrace errorThunk
    Just fin -> String (Fin.unlift fin)
  Json.Object mbrs -> Object $ Contiguous.map'
    ( \(Json.Member{key,value}) -> case Map.lookup key ks of
      Nothing -> errorWithoutStackTrace errorThunk
      Just fin -> Member
        { key = Fin.unlift fin
        , value = applyDictionaries ks strs value
        }
    ) mbrs

data Counters = Counters
  { keys :: !(Map ShortText Int)
  , strings :: !(Map ShortText Int)
  }

augmentCounters :: Counters -> Json.Value -> Counters
augmentCounters !cntrs@Counters{strings} = \case
  Json.Object xs -> foldl' augmentMemberCounters cntrs xs
  Json.Array xs -> foldl' augmentCounters cntrs xs
  Json.String t -> cntrs{strings=Map.insertWith (+) t 1 strings}
  _ -> cntrs

augmentMemberCounters :: Counters -> Json.Member -> Counters
augmentMemberCounters !cntrs@Counters{keys} (Json.Member{key,value}) =
  augmentCounters
    (cntrs{keys=Map.insertWith (+) key 1 keys})
    value

-- Create a vector of tokens ordered by cardinality.
toTexts :: Int -> Map Int (Set ShortText) -> Unlifted.Vector 'Unknown ShortText#
toTexts !sz m = runST do
  dst <- UVec.uninitialized sz
  let step g !txts = \ !ix0 -> do
        !ix1 <- foldlM
          (\ !ix !txt -> do
            Unlifted.unsafeWrite dst ix (TS.unlift txt)
            pure (ix + 1)
          ) ix0 txts
        g ix1
  total <- Map.foldl step pure m 0
  if total /= sz
    then unsafeIOToST (throwIO JuiceLengthMismatch)
    else Unlifted.unsafeFreeze dst

data JuiceLengthMismatch = JuiceLengthMismatch
  deriving stock (Eq,Show)
  deriving anyclass (Exception)

errorThunk :: a
errorThunk = errorWithoutStackTrace "Juice: mistake"

compress :: Json.Value -> CompressedValue
compress !v =
  let !keyLen = Unsafe.Nat keysCard in
  let !ks = Unlifted.unsafeCoerceLength keyLen keyVec in
  let !strLen = Unsafe.Nat strsCard in
  let !ss = Unlifted.unsafeCoerceLength strLen strVec in
  let !keyMap = Unlifted.ifoldl' ins Map.empty (Unlifted.slice keyLen ks) in
  let !strMap = Unlifted.ifoldl' ins Map.empty (Unlifted.slice strLen ss) in
  CompressedValue keyLen strLen
    (Dictionaries ks ss)
    (applyDictionaries keyMap strMap v)
  where
  Counters{keys,strings} = augmentCounters (Counters Map.empty Map.empty) v
  keysCard = Map.size keys
  strsCard = Map.size strings
  keyGroups = groupByCount keys
  strGroups = groupByCount strings
  keyVec = toTexts keysCard keyGroups
  strVec = toTexts strsCard strGroups

compressMany :: SmallArray Json.Value -> CompressedValues
compressMany !vs =
  let !keyLen = Unsafe.Nat keysCard in
  let !ks = Unlifted.unsafeCoerceLength keyLen keyVec in
  let !strLen = Unsafe.Nat strsCard in
  let !ss = Unlifted.unsafeCoerceLength strLen strVec in
  let !keyMap = Unlifted.ifoldl' ins Map.empty (Unlifted.slice keyLen ks) in
  let !strMap = Unlifted.ifoldl' ins Map.empty (Unlifted.slice strLen ss) in
  CompressedValues keyLen strLen
    (Dictionaries ks ss)
    (Contiguous.map' (applyDictionaries keyMap strMap) vs)
  where
  Counters{keys,strings} =
    foldl' augmentCounters (Counters Map.empty Map.empty) vs
  keysCard = Map.size keys
  strsCard = Map.size strings
  keyGroups = groupByCount keys
  strGroups = groupByCount strings
  keyVec = toTexts keysCard keyGroups
  strVec = toTexts strsCard strGroups

ins ::
     Map ShortText (Fin n)
  -> Fin n
  -> ShortText#
  -> Map ShortText (Fin n)
ins m ix t = Map.insert (TS.lift t) ix m

groupByCount :: Map ShortText Int -> Map Int (Set ShortText)
groupByCount = Map.foldlWithKey'
  ( \acc str count ->
    Map.insertWith Set.union count (Set.singleton str) acc
  ) Map.empty
