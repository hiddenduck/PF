module Ficha7 where

--1
data ExpInt = Const Int
 | Simetrico ExpInt
 | Mais ExpInt ExpInt
 | Menos ExpInt ExpInt
 | Mult ExpInt ExpInt
{-            Mais
            /      \
        Const 3     Menos
                /         \
           Const 2       Const 5
-}
-- 3 + (2 - 5)
a1 = Mais (Const 3) (Menos (Const 2) (Const 5))
a2 = Menos (Const 1) (Const 3)
--a)
calcula :: ExpInt -> Int
calcula er = case er of
 Const n     -> n
 Simetrico e -> - calcula e
 Mais e1 e2  -> (calcula e1) + (calcula e2)
 Menos e1 e2 -> (calcula e1) - (calcula e2)
 Mult e1 e2  -> (calcula e1) * (calcula e2)
--b)
infixa :: ExpInt -> String
infixa expr = case expr of
 Const n     -> show n
 Simetrico e -> "-(" ++ infixa e ++ ")"
 Mais e1 e2  -> "(" ++ (infixa e1) ++ "+" ++ (infixa e2) ++ ")"
 Menos e1 e2 -> "(" ++ (infixa e1) ++ "-" ++ (infixa e2) ++ ")"
 Mult e1 e2  -> "(" ++ (infixa e1) ++ "*" ++ (infixa e2) ++ ")"
--c)
posfixa :: ExpInt -> String
posfixa expr = case expr of
 Const n     -> show n ++ " "
 Simetrico e -> "-" ++ posfixa e
 Mais e1 e2  -> posfixa e1 ++ posfixa e2 ++ "+ "
 Menos e1 e2 -> posfixa e1 ++ posfixa e2 ++ "- "
 Mult e1 e2  -> posfixa e1 ++ posfixa e2 ++ "* "

--2
data RTree a = R a [RTree a] deriving Show
{-        R 10
      /    |    \    \
  [  5     4     2    1  ]
   /  \    |     |    |
 [12  13] [ ]   [ ]  [ ]
  |   | 
 [ ] [ ]
-}
a3 = R 10 [R 5 [R 12 [], R 13 []], R 4 [], R 2 [], R 1 []] 
--a)
soma :: Num a => RTree a -> a
soma (R n l) = n + sum(map soma l)
--b)
altura :: RTree a -> Int
altura (R n []) = 1
altura (R n l) = 1 + maximum (map altura l)
--c)
prune :: Int -> RTree a -> RTree a
prune 0 (R n l) = R n []
prune x (R n l) = R n (map (prune (x - 1)) l)
--d)
mirror :: RTree a -> RTree a
mirror (R n l)  = R n (reverse (map mirror l))
--e)
postorder :: RTree a -> [a]
postorder (R n l) = concat(map postorder l) ++ [n]

--3
data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)
--a)
