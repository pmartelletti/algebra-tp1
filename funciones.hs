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
--elementosRepetidos (x:xs) y | x `elem` y = 1 + elementosRepetidos xs y
--elementosRepetidos (x:xs) y = elementosRepetidos xs y
elementosRepetidos (x:xs) (y:ys) | x == y = 1 + elementosRepetidos xs ys
elementosRepetidos (x:xs) y = elementosRepetidos xs y

-- funcione auxiliares para los otros ejercicios
-- encuentra el primer divisor en una lista (no tiene por que ser el menor)
primerDivisorEn :: [Int] -> Int -> Int
primerDivisorEn [] n = 0
primerDivisorEn (x:xs) n | ((n `mod` x) == 0) = x
primerDivisorEn (x:xs) n = primerDivisorEn xs n

-- tecnica similar a la criba de erastotenes, pero para un solo numero
-- se que siempre es el menor porque la lista [2..] esta ordenada de menor a mayor
menorPrimoDivisor :: Int-> Int
menorPrimoDivisor x = primerDivisorEn [2..] x


-- ejercicio 5
factoresPrimos :: Int -> [Int]
factoresPrimos n | n < 2 = []
-- puedo usar division entera porque se que p divide a n porque p es primo
factoresPrimos n = p : factoresPrimos(n `div` p)
		where p = menorPrimoDivisor n
