s1 :: Integer -> Integer
s1 1 = (2*1 - 1)^2
s1 n = s1(n-1) + (2*n-1)^2

s2 :: Integer -> Integer
s2 1 = ((-1)^1) * 2^1
s2 n = s2(n-1) + ((-1)^n) * 2^n
