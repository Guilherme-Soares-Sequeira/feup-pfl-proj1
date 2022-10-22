module Polinomio (Polinomio,
                add, mul, deriv, Polinomio.sum,
                norm, Polinomio.toReadable) where
import qualified Monomio
import Nat
import qualified Data.List
import LiteralMap
import Utils

import Data.Char

type Polinomio = [Monomio.Monomio]

data Token
  = PlusTok
  | MinusTok
  | ExpoTok
  | IncogTok Char
  | DoubleTok Double
  deriving (Show)

lexer :: String -> [Token]
lexer []              = []
lexer ('+' : restStr) = PlusTok  : lexer restStr
lexer ('*' : restStr) = lexer restStr 
lexer ('^' : restStr) = ExpoTok : lexer restStr
lexer ('-' : restStr) = PlusTok : MinusTok : lexer restStr
lexer (chr : restStr) 
  | isSpace chr       = lexer restStr
lexer str@(chr : rest)
  | isDigit chr = DoubleTok (read digitStr) : lexer restStr
  | isAlpha chr = IncogTok (toLower chr) : lexer rest
  where
      (digitStr, restStr) = (getDouble str, drop (length (getDouble str)) str)
  -- runtime error for all other characters:
lexer (chr : restString) 
  = error ("lexer: unexpected character: '" ++ show chr ++ "'")

type Coef = Double
type Expo = Double

data Literal
  = ComplexIncog Char Expo
  | Empty
  | Mul Literal Literal
  deriving (Show)

data Variable 
  = CoefAndLiteral Coef Literal
  | NCoefAndLiteral Coef Literal
  deriving (Show)

--data Expr
--  = Mon    Variable     -- double constants, leaves of the expression tree
--  | Add    Expr Expr    -- addition node
--  deriving (Show)

type Expr = [Variable]

parseChar :: [Token] -> Maybe (Char, [Token])
parseChar (IncogTok n : restTokens)
  = Just (n, restTokens)
parseChar tokens
  = Nothing

parseDouble :: [Token] -> Maybe (Double, [Token])
parseDouble (DoubleTok n : restTokens)
  = Just (n, restTokens)
parseDouble tokens
  = Nothing

parseCharOrExpo :: [Token] -> Maybe (Literal, [Token])
parseCharOrExpo tokens
  = case parseChar tokens of
      Just (incog, ExpoTok : restTokens1) -> 
          case parseDouble restTokens1 of
            Just (expo, restTokens2) -> Just (ComplexIncog incog expo, restTokens2)
            Nothing -> Nothing
      Just (incog, nextTok : restTokens1) -> case nextTok of
        IncogTok next -> case parseCharOrExpo (nextTok : restTokens1) of
          Just (literal, restTokens2) -> Just (Mul (ComplexIncog incog 1.0) literal, restTokens2)   
          result -> Nothing
        result -> Nothing
      Just (incog, restTokens1) -> Just (ComplexIncog incog 1.0, restTokens1)
      result -> Nothing     -- could be 'Nothing' or a valid expression

parseCoefOrIncog :: [Token] -> Maybe (Variable, [Token])
--parseCoefOrIncog [] = Just (CoefAndLiteral 0.0 Empty, [])
parseCoefOrIncog (tok : rest) =
  case tok of
    DoubleTok n -> case parseDouble (tok : rest) of
      Just (coef, nextTok: restTokens1) -> case nextTok of
        IncogTok c -> case parseCharOrExpo restTokens1 of
          Just(literal, restTokens2) -> Just (CoefAndLiteral coef literal, restTokens1)
          result -> Nothing
        result -> Nothing
      result -> Nothing
    IncogTok c -> case parseCharOrExpo (tok : rest) of
      Just (literal, restTokens) -> Just (CoefAndLiteral 1.0 literal, restTokens)
      result -> Nothing
    result -> Nothing
  
parse :: [Token] -> Maybe Expr
parse tokens
  = case parseCoefOrIncog tokens of
      Just (var1, PlusTok : restTokens1) -> 
          case parseCoefOrIncog restTokens1 of
            Just (var2, restTokens2) -> Just [var1, var2]
            Nothing                  -> Nothing
      Just(var1, []) -> Just [var1]
      result -> Nothing

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
toReadable [] = "0"
toReadable [x] = Monomio.toReadable x
toReadable (x:rest) = Monomio.toReadable x ++ " " ++ Data.List.intercalate " " [Monomio.toReadableWithSign x | x <- rest]


monomio1 = (1.0, LiteralMap.fromList [('x', intToNat 1), ('y', intToNat 1)])
monomio2 = (4.0, LiteralMap.fromList [('x', intToNat 1), ('y', intToNat 1)])
monomio3 = (3.5, LiteralMap.fromList [('x', intToNat 1), ('y', intToNat 1)])
monomio4 = (-2.5, LiteralMap.fromList [('z', intToNat 1), ('y', intToNat 2)])
monomio5 = (12.5, LiteralMap.fromList [('z', intToNat 1), ('y', intToNat 2)])
polinomio1 = [monomio1, monomio2, monomio3]
polinomio2 = [monomio3, monomio4, monomio5]

--     MinusTok -> case nTok of
--      DoubleTok n -> case parseDouble nRest of
--        Just (coef, nextTok: restTokens1) -> case nextTok of
--          IncogTok c -> case parseCharOrExpo restTokens1 of
--            Just(literal, restTokens2) -> Just (NCoefAndLiteral coef literal, restTokens1)
--            result -> Nothing
--          result -> Nothing
--        result -> Nothing
--      IncogTok c -> case parseCharOrExpo nRest of
--        Just (literal, restTokens) -> Just (NCoefAndLiteral 1.0 literal, restTokens)
--        result -> Nothing

-- parse :: String -> Maybe Expr
-- lexer input -> tokens
-- split por PlusTok
-- membros -> monoms

--parse2 :: String -> Polinomio
--parse2 = toInternal . toVariables . split . lexer

split :: [Token] -> [[Token]]
split [] = []
split (x : PlusTok : xs) = [x] : split xs

--toInternal :: [[Token]] -> Polinomio
--toInternal = map (toMonomio . toVar)

-- toVariables :: [[Token]] -> [Variable]
-- toVariables = map toVariable
-- 
-- toVariable :: [Token] -> Variable
-- toVariable [IncogTok var] = CoefAndLiteral 1.0 (ComplexIncog var 1)
-- toVariable [IncogTok var : ExpoTok : DoubleTok exp] = CoefAndLiteral 1.0 (ComplexIncog var exp)


--toMonomio :: Variable -> Monomio.Monomio
