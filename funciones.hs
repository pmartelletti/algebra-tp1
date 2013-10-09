-- ejercicio 1
s1 :: Int -> Int
s1 1 = (2*1 - 1)^2
s1 n = s1(n-1) + (2*n-1)^2

-- ejercicio 2
s2 :: Int -> Int
s2 1 = ((-1)^1) * 2^1
s2 n = s2(n-1) + ((-1)^n) * 2^n

-- funcion que dada una lista, dice si el el elemento y está al menos una vez en la misma
pertenece :: [Int] -> Int -> Bool
pertenece [] y = False
pertenece (x:xs) y | x == y = True
                   | x /= y = pertenece xs y

-- ejercicio 3
elementosRepetidos :: [Int] -> [Int] -> Int
elementosRepetidos x [] = 0
elementosRepetidos [] y = 0
elementosRepetidos (x:xs) y | pertenece y x = 1 + elementosRepetidos xs y
elementosRepetidos (x:xs) y = elementosRepetidos xs y

-- devuelve la longitud de una lista
long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + (long xs)

-- funcion que decide si una lista es decreciente
decreciente :: [Int] -> Bool
decreciente [] = True
decreciente x | long(x) == 1 = True
decreciente (x:y:ys) | x >= y = decreciente (y:ys)
decreciente x = False

-- ejercicio 4
triangular :: [Int] -> Bool
triangular (x:y:ys) | x <= y = triangular(y:ys)
		    | x > y = decreciente(y:ys)))


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
