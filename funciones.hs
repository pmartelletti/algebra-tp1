import Data.List
import Data.Maybe

-- ejercicio 1
s1 :: Int -> Int
s1 1 = (2*1 - 1)^2
s1 n = s1(n-1) + (2*n-1)^2

-- ejercicio 2
s2 :: Int -> Int
s2 1 = ((-1)^1) * 2^1
s2 n = s2(n-1) + ((-1)^n) * 2^n

-- ejercicio 3
elementosRepetidos :: [Int] -> [Int] -> Int
elementosRepetidos x [] = 0
elementosRepetidos [] y = 0
elementosRepetidos (x:xs) y | x `elem` y = 1 + elementosRepetidos xs y
elementosRepetidos (x:xs) y = elementosRepetidos xs y

-- funciones auxiliares
esPrimo :: Integer -> Bool
esPrimo x = null (filter (\y ->  x `mod`y == 0) (takeWhile (\y ->  y*y <= x) [2..]))

-- tecnica similar a la criba de erastotenes, pero para un solo numero
menorPrimoDivisor :: Integer-> Integer
menorPrimoDivisor x = fromJust (find(\y -> (x `mod` y) == 0 ) [2..])


-- ejercicio 5
factoresPrimos :: Integer -> [Integer]
factoresPrimos n | n < 2 = []
-- puedo usar division entera porque se que p divide a n porque p es primo
factoresPrimos n = p : factoresPrimos(n `div` p)
		where p = menorPrimoDivisor n

