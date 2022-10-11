module Nat (Nat,
             natToInt, intToNat) where

data Nat = Zero | Suc Nat
  deriving Eq

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n-1))

instance Show Nat where
  show n = show (natToInt n)