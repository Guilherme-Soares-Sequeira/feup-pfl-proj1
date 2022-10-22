module Utils (getDouble) where

import Data.Char

getDoubleHelper :: Bool -> String -> String
getDoubleHelper _ [] = ""
getDoubleHelper False (x:xs)
  | isDigit x = x : getDoubleHelper False xs
  | x == '.' = x : getDoubleHelper True xs
  | otherwise = ""
getDoubleHelper True (x:xs) = if isDigit x then x : getDoubleHelper True xs else ""

getDouble :: String -> String
getDouble = getDoubleHelper False
