data Nat = Zero | Suc Nat
  deriving Eq

intFromNat :: Nat -> Int
intFromNat Zero = 0
intFromNat (Suc n) = 1 + intFromNat n

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n-1))

instance Show Nat where
  show n = show (intFromNat n)

type Expoente = Nat
type Coeficiente = Double
type Incognita = Char
type Literal = (Incognita, Expoente)
type Monomio = (Coeficiente, [Literal])
type Polinomio = [Monomio]

monomAdd :: Monomio -> Monomio -> Monomio
monomAdd (coef1, literal1) (coef2, literal2)
  | (literal1 /= literal2) = error "Monomios nao tem o mesmo expoente"
  | otherwise = (coef1+coef2, literal1)

getExponent :: Char -> [Literal] -> Int
getExponent incognita literais = head [intFromNat x | (incog, x) <- literais, incog == incognita]

monomDeriv :: Char -> Monomio -> Monomio
monomDeriv incognita (coeficiente, literais) = (coeficiente, literais)

-- 7x²y³ + 5x²y³
-- [(+, 6, [(x,2), (y,3)]), (+, 5, [(x,2), (y, 3)])]