{-# LANGUAGE DataKinds, FlexibleInstances,
             GADTs, KindSignatures, StandaloneDeriving #-}

module Data.Number.NonStandard ( NonStandard, Z, S ) where

-- non-standard numbers with alpha^n = 0
-- (or equivalently: 0 != alpha = nth root of 0)

data {- kind -} Nat = Z | S Nat
type Z = 'Z
type S = 'S

data NonStandard :: Nat -> * -> * where
  Standard :: a -> NonStandard (S n) a
  Non :: NonStandard n a -> a -> NonStandard (S n) a

deriving instance Show a => Show (NonStandard n a)
instance Functor (NonStandard n) where
  fmap f (Standard a) = Standard $ f a
  fmap f (Non pre a) = Non (fmap f pre) (f a)

instance Num a => Num (NonStandard (S n) a) where
  (+) = add
  (*) = undefined
  negate = fmap negate
  signum = undefined
  fromInteger = Standard . fromInteger
  abs = Standard . abs . standardPart

standardPart :: NonStandard n a -> a
standardPart (Non rest _) = standardPart rest
standardPart (Standard a) = a

st = standardPart

add :: Num a => NonStandard n a -> NonStandard n a -> NonStandard n a
Standard a `add` Standard b = Standard $ a + b
Standard a `add` Non (Standard b') b = Non (Standard $ a + b') b
Standard a `add` Non pre@Non{} b = Non (Standard a `add` pre) b

-- Multiplication is n-truncated convolution
