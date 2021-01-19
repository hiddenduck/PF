module Teste1819 where
import Data.Char
import System.Random
--1
--a)
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a l = elemIndicesAux a l 0
elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux _ [] _ = []
elemIndicesAux a (h:t) acc | a == h = acc: elemIndicesAux a t (acc+1)
                           | otherwise = elemIndicesAux a t (acc+1)
--b)
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) | x == y = isSubsequenceOf xs ys
                              | otherwise = isSubsequenceOf (x:xs) ys

--2
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
arvore = Node ('a',1) Empty (Node ('b',2) Empty (Node ('c',1) Empty Empty))
--a)
lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP a (Node (r1,r2) e d)
 | a == r1 = Just r2
 | a < r1 = lookupAP a e
 | otherwise = lookupAP a d
--b)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

--3
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t)
 | isDigit h = (h:a,b)
 | isAlpha h = (a,h:b)
 | otherwise = (a,b)
  where
  (a,b) = digitAlpha t

--4
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
arvore2 =  (App (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) (Cons 1 Nil))
--a)
firstSeq :: Seq a -> a
firstSeq (Cons a _) = a
firstSeq (App Nil s) = firstSeq s
firstSeq (App a _) = firstSeq a
--b)
dropSeq :: Int -> Seq a -> Seq a
dropSeq _ Nil = Nil
dropSeq n (Cons _ b) = dropSeq (n - 1) b
dropSeq n (App a b)
 | n > n1 = dropSeq (n - n1) b
 | n == n1 = b
 | otherwise = App (dropSeq n a) b
  where
  n1 = contaCons a
contaCons :: Seq a -> Int
contaCons Nil = 0
contaCons (Cons _ b) = 1 + contaCons b
contaCons (App a b) = contaCons a + contaCons b 
--c)
instance Show a => Show (Seq a) where
 show x = "<<" ++ showAux x ++ ">>"

showAux :: Show a => Seq a -> String
showAux Nil = ""
showAux (Cons a Nil) = show a
showAux (Cons a b) = show a ++ "," ++ showAux b
showAux (App a b) = showAux a ++ "," ++ showAux b

--5
type Mat a = [[a]]
{-
|6 7 2|
|1 5 9|
|8 3 4|
-}
matA = [[6,7,2], [1,5,9], [8,3,4]]
--a)
getElem :: Mat a -> IO a
getElem mat = do let nMat = concat mat
                 let c = length nMat
                 x <- randomRIO(0,c-1)
                 return(nMat !! x)
--b)
magic :: Mat Int -> Bool
magic mat = linhas n mat && colunas n mat && diagonais n mat
 where
 n = sum(head mat)

linhas :: Int -> Mat Int -> Bool
linhas x = foldl(\acc c -> sum c == x && acc) True

colunas :: Int -> Mat Int -> Bool
colunas x m = foldl(\acc c -> sum c == x && acc) True (transpose m)

transpose :: Mat Int -> Mat Int
transpose ([]:_) = []
transpose m = map head m: transpose (map tail m)

diagonais :: Int -> Mat Int -> Bool
diagonais x m = sum m1 == x && sum m2 == x
 where
 m1 = diagonal1 m l1
 m2 = diagonal2 m l2
 l1 = [0..n]
 l2 = reverse l1
 n = length m - 1

diagonal1 :: Mat Int -> [Int] -> [Int]
diagonal1 [] _ = []
diagonal1 m (h:t) = (head m) !! h: diagonal1 (tail m) t

diagonal2 :: Mat Int -> [Int] -> [Int]
diagonal2 [] _ = []
diagonal2 m (h:t) = (head m) !! h: diagonal2 (tail m) t
