module Ficha6 where

data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

a1 = Node 5 (Node 3 Empty (Node 4 Empty Empty)) (Node 9 Empty (Node 10 Empty Empty))
{-
            5 
          /   \
         3     9
          \     \
           4     10
-}

--1a
altura :: BTree a -> Int
altura Empty        = 0
altura (Node r e d) = max (1 + altura e) (1 + altura d)
--1b
contaNodos :: BTree a -> Int
contaNodos Empty        = 0
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d
--1c
folhas :: BTree a -> Int
folhas Empty                = 0
folhas (Node _ Empty Empty) = 1
folhas (Node r e d)         = folhas e + folhas d
--1d
prune :: Int -> BTree a -> BTree a
prune 0 _                    = Empty
prune _ (Node _ Empty Empty) = Empty 
prune a (Node r e d)         = Node r (prune (a-1) e) (prune (a-1) e)

{-
            5 
          /   \
         3     9
          \     \
           4     10
-}
--1e
path :: [Bool] -> BTree a -> [a]
path _ Empty            = []
path [] (Node r _ _ )   = [r]
path (h:t) (Node r e d) = if h then r:path t d else r:path t e
--1f
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)
--1g
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _  = Empty
zipWithBT _ _ Empty  = Empty
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)  
--1h
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a a1 a2, Node b b1 b2, Node c c1 c2)
 where
 (a1,b1,c1) = unzipBT e
 (a2,b2,c2) = unzipBT d
a2 = Node (1,2,3) (Node (4,5,6) Empty Empty) (Node (9,0,1) Empty Empty)
{-
       (1,2,3)
       /      \
   (4,5,6)    (9,0,1)
-}

--2a
minimo :: Ord a => BTree a -> a
minimo (Node n Empty _) = n
minimo (Node n e _)     = minimo e
--2b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node n Empty _) = Empty
semMinimo (Node n e d )    = Node n (semMinimo e) d
--2c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node n Empty d) = (n,d)
minSmin (Node n e d)     = (m, Node n e' d)
 where
 (m,e') = minSmin e
--2d
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

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
 | Rep
 | Faltou
 deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

--3a
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (n,_,_,_) e d) = n == x || inscNum x (if x < n then e else d)
--3b
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nome,_,_) e d) = n == nome || inscNome n e || inscNome n d
--3c
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (numero,n,r,_) e d) = case r of
 TE -> trabEst e ++ [(numero,n)] ++ trabEst d
 _  -> trabEst e ++ trabEst d
--3d
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (numero,_,_,classificacao) e d) | n == numero = Just classificacao
                                             | n < numero = nota n e
                                             | otherwise = nota n d
--3e
percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas turma = somaDasFaltas (turma) / contaAlunos (turma) * 100
 where
 somaDasFaltas :: Turma -> Float
 somaDasFaltas (Node (_,_,_,classificacao) e d) = case classificacao of
  Faltou -> 1 + percFaltas e + percFaltas d 
  _      -> 0 + percFaltas e + percFaltas d
 contaAlunos :: Turma -> Float
 contaAlunos Empty = 0
 contaAlunos (Node n e d) = 1 + contaAlunos e + contaAlunos d
--3f
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = somaAprov turma / contaAprov turma
 where
 somaAprov :: Turma -> Float
 somaAprov (Node (_,_,_,classificacao) e d) = case classificacao of
  Aprov a -> fromIntegral a + somaAprov e + somaAprov d
  _       -> 0 + somaAprov e + somaAprov d
 contaAprov :: Turma -> Float
 contaAprov (Node (_,_,_,classificacao) e d) = case classificacao of
  Aprov a -> 1 + contaAprov e + contaAprov d
  _       -> 0 + contaAprov e + contaAprov d
--3g)
-- considero que alunos que faltam não são availiados 
aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = aprovados / avaliados
 where
 (aprovados, avaliados) = aprovAvAux turma
 aprovAvAux :: Turma -> (Float,Float)
 aprovAvAux Empty = (0,0)
 aprovAvAux (Node (_,_,_,classificacao) e d) = case classificacao of
  Aprov avaliacao -> (a+1,b)
  Rep             -> (a,b+1)
  _               -> (a,b)
  where
  (a,b)   = (a1+a2,b1+b2)
  (a1,b1) = aprovAvAux e
  (a2,b2) = aprovAvAux d 
