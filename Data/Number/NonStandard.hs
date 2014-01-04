{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, StandaloneDeriving #-}

module Data.Number.NonStandard ( NonStandard(..) ) where

data {- kind -} Nat = Z | S Nat

data NonStandard :: Nat -> * -> * where
  Standard :: a -> NonStandard (S n) a
  Non :: NonStandard n a -> a -> NonStandard (S n) a

deriving instance Show a => Show (NonStandard n a)
instance Functor (NonStandard n) where
  fmap f (Standard a) = Standard $ f a

instance Num a => Num (NonStandard (S n) a) where
  (+) = undefined
  (*) = undefined
  negate = fmap negate
  signum = undefined
  fromInteger = Standard . fromInteger
  abs = Standard . abs . standardPart

standardPart :: NonStandard n a -> a
standardPart (Non rest _) = standardPart rest
standardPart (Standard a) = a

st = standardPart
