module LiteralMap (LiteralMap,
                   insert, empty, isEmpty,
                   fromList, toList, insertMultiple,
                   leftmost, getExponent, reduceExponent, toReadable) where

import Nat
import Data.List (intercalate)

data LiteralMap k v = Empty
  | Node Char Nat (LiteralMap Char Nat) (LiteralMap Char Nat)
  deriving Show

instance Eq (LiteralMap k v) where
  Empty == Empty = True
  Node k v l r == Node k2 v2 l2 r2 = toList (Node k v l r) == toList (Node k2 v2 l2 r2)
  _ == _ = False

toReadable :: LiteralMap Char Nat -> String
toReadable mapa = intercalate "*" [if natToInt expo /= 1 then [incog] ++ "^" ++ show (natToInt expo) else [incog] | (incog, expo) <- toList mapa]

insert :: (Char, Nat) -> LiteralMap Char Nat -> LiteralMap Char Nat
insert (incog, expo) Empty = Node incog expo Empty Empty
insert (incog, expo) (Node incog2 expo2 l r)
  | incog == incog2 = Node incog (intToNat (natToInt expo + natToInt expo2)) l r
  | incog > incog2 = Node incog2 expo2 l (insert (incog, expo) r)
  | incog < incog2 = Node incog2 expo2 (insert (incog, expo) l) r

insertMultiple :: [(Char, Nat)] -> LiteralMap Char Nat -> LiteralMap Char Nat
insertMultiple [] mapa = mapa
insertMultiple list Empty = fromList list
insertMultiple (elem : rest) mapa = insert elem (insertMultiple rest mapa)

empty :: LiteralMap Char Nat
empty = Empty

isEmpty :: LiteralMap Char Nat -> Bool
isEmpty Empty = True
isEmpty _ = False

fromList :: [(Char, Nat)] -> LiteralMap Char Nat
fromList = foldr insert Empty

toList :: LiteralMap Char Nat -> [(Char, Nat)]
toList Empty = []
toList (Node k v l r) = toList l ++ [(k, v)] ++ toList r

leftmost :: LiteralMap Char Nat -> (Char, Nat)
leftmost (Node k v Empty _) = (k, v)
leftmost (Node _ _ left _) = leftmost left

getExponent :: Char -> LiteralMap Char Nat -> Nat
getExponent x Empty  = intToNat 0
getExponent x (Node incog expo l r)
  | x == incog = expo
  | x < incog = getExponent x l
  | x > incog = getExponent x r

delete :: Char -> LiteralMap Char Nat -> LiteralMap Char Nat
delete x Empty = Empty
delete x (Node incog expo Empty right)
  | x==incog = right
delete x (Node incog expo left Empty)
  | x==incog = left
delete x (Node incog expo left right)
  | x<incog = Node incog expo (delete x left) right
  | x>incog = Node incog expo left (delete x right)
  | x==incog = let z = leftmost right
            in Node (fst z) (snd z) left (delete (fst z) right)



reduceExponent :: Char -> LiteralMap Char Nat -> LiteralMap Char Nat
reduceExponent x Empty = Empty
reduceExponent x (Node incog expo left right)
  | x == incog && expo == intToNat 1 = delete x (Node incog expo left right)
  | x == incog = Node incog (intToNat (natToInt expo - 1)) left right
  | x < incog = Node incog expo (reduceExponent x left) right
  | x > incog = Node incog expo left (reduceExponent x right)
