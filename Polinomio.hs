module Polinomio (Polinomio,
                add, mul, deriv, Polinomio.sum,
                norm, Polinomio.toReadable) where
import qualified Monomio
import Nat
import qualified Data.List
import LiteralMap

import Data.Char

type Polinomio = [Monomio.Monomio]

data Token
  = PlusTok
  | MinusTok
  | TimesTok
  | OpenTok
  | CloseTok
  | ExpoTok
  | IncogTok Char
  | DoubleTok Double
  deriving (Show)

lexer :: String -> [Token]
lexer []              = []
lexer ('+' : restStr) = PlusTok  : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr 
lexer ('(' : restStr) = OpenTok  : lexer restStr 
lexer (')' : restStr) = CloseTok : lexer restStr
lexer ('^' : restStr) = ExpoTok : lexer restStr
lexer ('-' : restStr) = MinusTok : lexer restStr
lexer (chr : restStr) 
  | isSpace chr       = lexer restStr
lexer str@(chr : _) 
  | isDigit chr
  = DoubleTok (read digitStr) : lexer restStr
  | isAlpha chr = IncogTok (toLower chr) : lexer restStr
  where
      (digitStr, restStr) = (myfunc 0 str, drop (length (myfunc 0 str)) str)
      -- local function to convert a string to a double value
      myfunc :: Int -> String -> String
      myfunc 0 [] = ""
      myfunc 1 [] = ""
      myfunc 0 (x:xs) = if isDigit x then x : myfunc 0 xs else x : myfunc 1 xs   
      myfunc 1 (x:xs) = if isDigit x then x : myfunc 1 xs else ""

  -- runtime error for all other characters:
lexer (chr : restString) 
  = error ("lexer: unexpected character: '" ++ show chr ++ "'")

sum :: Polinomio -> Monomio.Monomio -- Assumes all elements have the same literal
sum [x] = x
sum [] = (0.0, LiteralMap.empty)
sum (x:rest) = x `Monomio.add` Polinomio.sum rest

norm :: Polinomio -> Polinomio
norm [] = []
norm [xs] = [xs | fst xs /= 0.0]
norm (x:rest) = filter (\x -> fst x /= 0.0) [Polinomio.sum [elem | elem <- rest, snd elem == snd x ] `Monomio.add` x] ++ norm [Polinomio.sum [elem | elem <- rest, snd elem /= snd x ]]

add :: Polinomio -> Polinomio -> Polinomio
add pol1 pol2 = norm (pol1 ++ pol2)

mul :: Polinomio -> Polinomio -> Polinomio
mul pol1 pol2 = norm [x `Monomio.mul` y| x <- norm pol1, y <- norm pol2]

deriv :: Char -> Polinomio -> Polinomio
deriv incog pol = norm [Monomio.deriv incog x | x <- norm pol]

toReadable :: Polinomio -> String
toReadable [] = ""
toReadable [x] = Monomio.toReadable x
toReadable (x:rest) = Monomio.toReadable x ++ " " ++ Data.List.intercalate " " [Monomio.toReadableWithSign x | x <- rest]


monomio1 = (1.0, LiteralMap.fromList [('x', intToNat 1), ('y', intToNat 1)])
monomio2 = (4.0, LiteralMap.fromList [('x', intToNat 1), ('y', intToNat 1)])
monomio3 = (3.5, LiteralMap.fromList [('x', intToNat 1), ('y', intToNat 1)])
monomio4 = (-2.5, LiteralMap.fromList [('z', intToNat 1), ('y', intToNat 2)])
monomio5 = (12.5, LiteralMap.fromList [('z', intToNat 1), ('y', intToNat 2)])
polinomio1 = [monomio1, monomio2, monomio3]
polinomio2 = [monomio3, monomio4, monomio5]