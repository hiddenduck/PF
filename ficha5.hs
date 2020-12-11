module Ficha5 where
import Data.List
--1
--a)
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (h:t) = if f h then True else any' f t
--b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys
--c)
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:t) = if f h then h: takeWhile' f t else []
--d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (h:t) = if f h then dropWhile' f t else (h:t)
--e)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t) = if f h then (h:x,y) else ([],h:y)
 where
 (x,y) = span' f t
--f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' f x (h:t) = if f x h then t else h: deleteBy' f x t
--g)
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' f (h:t) = insertOn' f h (sortOn' f t)
insertOn' :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn' _ y [] = [y]
insertOn' f y (h:t) | (f y) <= (f h) = y:h:t
                    | otherwise = h: insertOn' f y t

--2
type Polinomio = [Monomio]
type Monomio = (Float,Int)
--a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n = filter (\x -> snd x == n)
--b)
conta :: Int -> Polinomio -> Int
conta g = foldl (\acc m -> if (g == snd m) then acc+1 else acc) 0
--c)
grau :: Polinomio -> Int
grau ((_,x):t) = ordem (\max (_,g) -> if (max >= g) then max else g) x t
 where
 ordem _ max [] = max
 ordem f max (h:t) = ordem f (f max h) t
--d)
deriv :: Polinomio -> Polinomio
deriv = map(\(x,y) -> (x*fromIntegral y, y-1))
--e)
calcula :: Float -> Polinomio -> Float
calcula x = foldl (\acc (c,g) -> c*(x^g) + acc) 0
--f)
simp :: Polinomio -> Polinomio
simp = filter (\(c,_) -> c /= 0)
--g)
mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) = map(\(c,g) -> (x*c,y+g))
--h)
ordena :: Polinomio -> Polinomio
ordena = sortOn snd
--i)
normaliza :: Polinomio -> Polinomio
normaliza p = foldl f [] (ordena p)
 where
 f [] x = [x]
 f ((c,g):t) (x,y)
  | g == y = (c+x,y):t
  | otherwise = (x,y):(c,g):t
--j)
soma :: Polinomio -> Polinomio -> Polinomio
soma l1 l2 = normaliza (l1++l2)
--k)
produto :: Polinomio -> Polinomio -> Polinomio
produto l1 l2 = concat(map(\m -> mult m l2) l1)
--l
equiv :: Polinomio -> Polinomio -> Bool
equiv l1 l2 = ordena(normaliza l1) == ordena(normaliza l2)

--3
type Mat a = [[a]]
--a) 
dimOK :: Mat a -> Bool
dimOK (l:ls) = all(\x -> length x == length l) ls
--b)
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (l:ls) = (length (l:ls), length l)
--c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith(zipWith(+))
--d)
transpose' :: Mat a -> Mat a
transpose' ([]:_) = [] -- se uma das linhas chegar a lista vazia tudo tem de ficar lista vazia
transpose' l = map head l: transpose'(map tail l)
--e)
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = [[sum(zipWith(*) l1 l2) | l2 <- transpose m2]| l1 <- m1]
--f)
zipWMat:: (a->b->c)->Mat a -> Mat b -> Mat c
zipWMat _ _ [] = []
zipWMat _ [] _ = []
zipWMat f (m1:m1s) (m2:m2s) = combinaLinhas f m1 m2 : zipWMat f m1s m2s
combinaLinhas :: (a->b->c) -> [a] -> [b] -> [c]
combinaLinhas _ [] _ = []
combinaLinhas _ _ [] = []
combinaLinhas f (l1:l1s) (l2:l2s) = f l1 l2 : combinaLinhas f l1s l2s

--ou
--zipWMat' f = zipWith (zipWith f)

somaMatrizes = zipWMat (+)

--g)
triSup :: (Num a, Eq a) => Mat a -> Bool
triSup m = l == c && all (\(p,l) -> aux p l) (zip [0..l-1] m)
 where 
 (l,c) = dimMat m 

aux :: (Num a, Eq a) => Int -> [a] -> Bool
aux 0 _ = True
aux _ [] = False
aux n (h:t) = h == 0 && aux (n-1) t

--h)
rotateLeft :: Mat a -> Mat a
rotateLeft l = reverse(transpose' l)