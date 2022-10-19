module Monomio (Monomio,
                add, mul, deriv, getExponent,
                reduceExponent, toReadable) where

import qualified LiteralMap
import Nat
import Data.Char (intToDigit)
import qualified LiteralMap

type Coeficiente = Double
type Monomio = (Coeficiente, LiteralMap.LiteralMap Char Nat)

toReadable :: Monomio -> String
toReadable (coef, map)
  | LiteralMap.isEmpty map = show coef 
  | otherwise = show coef ++ "*" ++ LiteralMap.toReadable map

add :: Monomio -> Monomio -> Monomio
add (coef1, literal1) (coef2, literal2)
  | literal1 /= literal2 = error "Monomios nao tem o mesmo expoente"
  | otherwise = (coef1+coef2, literal1)

getExponent :: Char -> Monomio -> Nat
getExponent incognita (exponent, map) = LiteralMap.getExponent incognita map

mul :: Monomio -> Monomio -> Monomio
mul (coef1, literais1) (coef2, literais2) = (coef1 * coef2, LiteralMap.insertMultiple (LiteralMap.toList literais2) literais1)

deriv :: Char -> Monomio -> Monomio
deriv incognita (coeficiente, literais)
  | natToInt (LiteralMap.getExponent incognita literais) == 0 = (0.0, LiteralMap.empty)
  | otherwise = ( fromIntegral (natToInt (LiteralMap.getExponent incognita literais)) * coeficiente, LiteralMap.reduceExponent incognita literais)

reduceExponent :: Char -> Monomio -> Monomio --TODO see if it's useful
reduceExponent incog (coef, map) = (coef, LiteralMap.reduceExponent incog map)