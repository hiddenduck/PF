module Ficha4 where
import Data.Char
--1
--a)
--[x | x <- [1..20], mod x 2 == 0, mod x 3 == 0] -> [6,12,18]
alineaA :: Int -> [Int]
alineaA x  | x == 20 = []
           | (mod x 2 == 0 && mod x 3 == 0) = x: alineaA (x+1)
           | otherwise = alineaA (x+1)

alineaA' :: [Int] -> [Int]
alineaA' [] = []
alineaA' (h:t) | (mod h 2 == 0) && (mod h 3 == 0) = h: alineaA' t
               | otherwise = alineaA' t
--b)
--[x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0] -> [6,12,18]
alineaB :: Int -> Int -> [Int]
alineaB x y | x == y = []
            | (mod x 2 == 0 && mod x 3 == 0) = x: alineaB (x+1) y
            | otherwise = alineaB (x+1) y
--c)
--[(x,y) | x <- [0..20], y <- [0..20], x+y == 30] -> [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
--[(x,y) | x <- [0..20], y <- [30 - x]]
--d)
--[sum [y | y <- [1..x], odd y] | x <- [1..10]]
--[1,1,4,4,9,9,16,16,25,25]
--[x^2 | x <- [1..5]] ++ [x^2 | x <- [1..5]]
--2
--a)
dupl :: [Int]
dupl = [2^x | x <- [0..10]]
--b)
execb = [(x,y) | x <- [1..5], y <- [1..5], x + y == 6]
--ou
execb' = [(x,6-x) | x <- [1..5]]
--c)
execc = [[x | x <- [1..y]] | y <- [1..5]]
--d)
execd = [[1 | x <- [1..y]] | y <- [1..5]]
--e)
exece = [fact x | x <- [1..6]]
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)
--3
digitAlpha :: String -> (String, String)
digitAlpha "" = ("","")
digitAlpha (h:t) | isDigit h = (h:x,y)
                 | isAlpha h = (x,h:y)
                 | otherwise = (x, y)
 where
 (x,y) = digitAlpha t
--4
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h < 0     = (a+1,b,c)
          | h > 0     = (a,b,c+1) 
          | otherwise = (a,b+1,c)
 where
 (a,b,c) = nzp t
--5
divMod':: Integral a => a->a->(a,a)
divMod' _ 0 = error "divisão por 0"
divMod' x y
    | (x>0 && y<0) || (x<0 && y>0) = (q1-1,r1)
    | x>0 && y>0 && x<y = (0,x)
    | x<0 && y<0 && x>y = (0,x)
    | otherwise = (q2+1,r2)
        where 
            (q1,r1) = divMod' (x+y) y
            (q2,r2) = divMod' (x-y) y
--6
{- 
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t
Note que
fromDigits [1,2,3,4] = 1 × 10^3 + 2 × 10^2 + 3 × 10^1 + 4 × 10^0
= 4 + 10 × (3 + 10 × (2 + 10 × (1 + 10 × 0)))
-}
fromDigits :: [Int] -> Int
fromDigits l = fromDigitsAux l 0
fromDigitsAux :: [Int] -> Int -> Int
fromDigitsAux [] acc    = acc
fromDigitsAux (h:t) acc = fromDigitsAux t (h + 10 * acc)
--7
{-
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]
-}
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit (h:t) = maxSumInitAux h h t
maxSumInitAux :: (Num a, Ord a) => a -> a -> [a] -> a
maxSumInitAux acc _ [] = acc
maxSumInitAux acc total (h:t) | acc > (total + h) = maxSumInitAux acc (total + h) t 
                              | otherwise = maxSumInitAux (total + h) (total + h) t
---8
fib n = fibAux n 0 1
fibAux :: Integer -> Integer -> Integer -> Integer
fibAux 0 acc_n _ = acc_n
fibAux n acc_n acc_nplus = fibAux (n-1) acc_nplus (acc_n + acc_nplus)