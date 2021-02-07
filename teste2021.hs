module Teste2021 where

--1
barrabarra :: Eq a => [a] -> [a] -> [a]
barrabarra [] _ = []
barrabarra l [] = l
barrabarra l (x:xs) = remover x (barrabarra l xs) 

remover :: Eq a => a -> [a] -> [a]
remover _ [] = []
remover a (x:xs) | a == x = xs
                 | otherwise = x: remover a xs
--2
--a)
type MSet a = [(a,Int)]
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a ((x,y):xs)
 | a == x && y == 1 = xs
 | a == x = (x,y-1):xs
 | otherwise = (x,y): removeMSet a xs
--b)
calcula :: MSet a -> ([a], Int)
calcula [] = ([],0)
calcula ((x,y):xs) = (x:a,y + b)
 where
 (a,b) = calcula xs

--3
partes :: String -> Char -> [String]
partes [] _ = [[]]
partes l a = partesAux l a []

partesAux :: String -> Char -> String -> [String]
partesAux [] _ acc = [acc]
partesAux (x:xs) a acc
 | x == a = acc: partesAux xs a []
 | otherwise = partesAux xs a (acc ++ [x])

--4
data BTree a = Empty | Node a (BTree a) (BTree a)
a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))
{-
            5
           / \
          3   7
               \
                9
-}
--a)
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove n (Node n' e d)
 | n < n' = Node n' (remove n e) d
 | n > n' = Node n' e (remove n d)
 | otherwise = case d of
  Empty -> e
  _     -> Node m e d'
  where
  (m,d') = minSmin d
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node n Empty d) = (n,d)
minSmin (Node n e d)     = (m, Node n e' d)
 where
 (m,e') = minSmin e

--b)
instance Show a => Show (BTree a) where
 show Empty = "*"
 show (Node r Empty Empty) = "(*" ++ " <-" ++ show r ++ "-> " ++ "*)"
 show (Node r e d) = "(" ++ show e ++ " <-" ++ show r ++ "-> " ++ show d ++ ")"

--5
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' f (x:xs) = insertOn' f x (sortOn' f xs)

insertOn' :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn' _ a [] = [a]
insertOn' f a (x:xs)
 | (f a) <= (f x) = a:x:xs
 | otherwise = x: insertOn' f a xs

--6
