--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | x > y = []
                | otherwise = x : enumFromTo' (x+1) y
--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | (x <= y) && (y <= z) = x : enumFromThenTo' y (2*y - x) z
                      | (x >= y) && (y >= z) = x : enumFromThenTo' y (2*y - x) z
                      | otherwise = [x]
--3
plusplus :: [a] -> [a] -> [a]
plusplus [] l = l
plusplus (h:t) l = h: plusplus t l
--4
excl :: [a] -> Int -> a
excl (h:t) 0 = h
excl (h:t) a = excl t (a-1)
--5
rev :: [a] -> [a]
rev [] = []
rev (h:t) = rev t ++ [h]
--6
tak :: Int -> [a] -> [a]
tak _ [] = []
tak 0 _ = []
tak a (h:t) = h : tak (a-1) t
--7
dro :: Int -> [a] -> [a]
dro _ [] = []
dro 0 l = l
dro a (h:t) = dro (a-1) t
--8
zi :: [a] -> [b] -> [(a,b)]
zi [] _ = []
zi _ [] = []
zi (x:xs) (y:ys) = (x,y): zi xs ys
--9
ele :: Eq a => a -> [a] -> Bool
ele _ [] = False
ele a (h:t) = if a == h then True else elem a t
--10
replicat :: Int -> a -> [a]
replicat 0 _ = []
replicat a b = b : replicat (a-1) b
--11
interper :: a -> [a] -> [a]
interper _ (x:[]) = [x]
interper a (h:t) = h:a:interper a t
--12
grou :: Eq a => [a] -> [[a]]
grou [] = []
grou (h:t) = gro [h] h t
 where
 gro hlist h [] = [hlist]
 gro hlist h (x:xs) | h == x = gro (hlist ++ [x]) x xs
                    | otherwise = hlist : gro [x] x xs
--13
conca :: [[a]] -> [a]
conca [] = []
conca (x:xs) = x ++ conca xs 
--14
ini :: [a] -> [[a]]
ini [] = [[]]
ini l = ini(init l) ++ [l]
--15
tai :: [a] -> [[a]]
tai [] = [[]]
tai l = l: tai(tail l)
--16
isPrefixO :: Eq a => [a] -> [a] -> Bool
isPrefixO [] _ = True
isPrefixO _ [] = False
isPrefixO (x:xs) (y:ys) = if x == y then isPrefixO xs ys else False
--17
isSuffixO :: Eq a => [a] -> [a] -> Bool
isSuffixO [] _ = True
isSuffixO _ [] = False
isSuffixO l1 l2 = if last l1 == last l2 then isSuffixO (init l1) (init l2) else False
--18
isSubsequenceO :: Eq a => [a] -> [a] -> Bool
isSubsequenceO [] _ = True
isSubsequenceO _ [] = False
isSubsequenceO (x:xs) (y:ys) = if x == y then isSubsequenceO xs ys else isSubsequenceO (x:xs) ys
--19
elemIndice :: Eq a => a -> [a] -> [Int]
elemIndice _ [] = []
elemIndice nu l = elemIndic nu l 0
 where
 elemIndic _ [] _ = []
 elemIndic x (y:ys) n = if x == y then [n] ++ elemIndic x ys (n+1) else elemIndic x ys (n+1)
--20
nu :: Eq a => [a] -> [a]
nu [] = []
nu (h:t) = h: nu(naux h t)
 where
 naux _ [] = []
 naux a (x:xs) = if a == x then naux a xs else x: naux a xs
--21
delet :: Eq a => a -> [a] -> [a]
delet _ [] = []
delet n (x:t) = if n == x then t else x: delet n t
--22
barbar :: Eq a => [a] -> [a] -> [a]
barbar [] _ = []
barbar l [] = l
barbar l1 (x:xs) = barbar (delet x l1) xs
--23
unio :: Eq a => [a] -> [a] -> [a]
unio [] l = l
unio l [] = l
unio l (y:ys) = if elem y l then unio l ys else [y] ++ unio l ys
--24
intersec :: Eq a => [a] -> [a] -> [a]
intersec [] _ = []
intersec _ [] = []
intersec (h:t) l2 = (intersectAux l2 h) ++ intersec t l2
 where
 intersectAux [] _ = []
 intersectAux (h:t) x
  | h == x = [h]
  | otherwise = intersectAux t x
--25
inser :: Ord a => a -> [a] -> [a]
inser x [] = [x]
inser x (h:t) = if x <= h then x:h:t else h: inser x t  
--26
unword :: [String] -> String
unword [] = ""
unword [x] = x
unword (h:t) = h ++ " " ++ unword t
--27
unline :: [String] -> String
unline [] = ""
unline (h:t) = h ++ "\n" ++ unline t
--28
pmaior :: Ord a => [a] -> Int
pmaior l = snd(pmaiorAux l)
pmaiorAux :: Ord a => [a] -> (a,Int)
pmaiorAux [x] = (x,0)
pmaiorAux (h:t) = if h > m then (h,0) else (h,p+1)
 where
 (m,p) = pmaiorAux t
--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos [x] = False
temRepetidos (x1:x2:t)
 | x1 == x2 = True
 | elem x1 t = True
 | elem x2 t = True
 | otherwise = temRepetidos t
--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) = if h >= '0' && h <= '9' then h:algarismos t else algarismos t
--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares (x1:x2:xs) = x2: posImpares xs
--32
posPares :: [a] -> [a]
posPares [] = []
posPares (x1:x2:xs) = x1: posPares xs 
--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (h:t) = loop [h] t
 where
 loop hlist [] = True
 loop hList (y:ys) = if (head hList <= y) then loop [y] ys else False

isSorted' :: Ord a => [a] -> Bool
isSorted' (x:y:xs) = if x <= y then isSorted' (y:xs) else False
isSorted' _ = True

isSortedXPTO :: Ord a => [a]->Bool
isSortedXPTO [] = True
isSortedXPTO (_:[]) = True
isSortedXPTO (h:y:t)
    | h <= y = isSortedXPTO t
    | otherwise = False
--34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]  
insert a (x:xs) | a <= x = a:x:xs
                | otherwise = x: insert a xs
--35
menor :: String -> String -> Bool
menor _ [] = False
menor [] _ = True
menor (x:xs) (y:ys) | x == y    = menor xs ys
                    | x < y     = True
                    | otherwise = False
--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a ((x,y):t) | a == x = True
                     | otherwise = elemMSet a t
--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):t) = y + lengthMSet t
--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):t) = x: converteMSet t 
converteMSet ((x,y):t) = x: converteMSet((x,y-1):t)
--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,y):t) | a == x = (x,y+1):t
                       | otherwise = (x,y):insereMSet a t
--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a ((x,y):t) | a == x && y == 1 = t
                       | a == x = (x,y-1):t
                       | otherwise = (x,y):removeMSet a t
--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = auxiliar 1 l
 where
 auxiliar i [x] = [(x,i)]
 auxiliar i (h1:h2:t) | h1 == h2 = auxiliar (i+1) (h1:t)
                      | otherwise = (h1,i): auxiliar 1 (h2:t)
--42
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers (h:t) = case h of
  Left h -> (h:x,y)
  Right h -> (x,h:y)
 where
 (x,y) = partitionEithers t
--43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of
  Just h  -> h:catMaybes t
  Nothing -> catMaybes t
--44
data Movimento = Norte | Sul | Este | Oeste deriving Show
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) = case h of
  Norte -> posicao(x,y+1) t
  Sul   -> posicao(x,y-1) t
  Este  -> posicao(x+1,y) t
  Oeste -> posicao(x-1,y) t
--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | (x1==x2) && (y1==y2) = []
                        | y2 > y1 = Norte: caminho (x1,y1+1) (x2,y2)
                        | y2 < y1 = Sul: caminho (x1,y1-1) (x2,y2)
                        | x2 < x1 = Oeste: caminho (x1-1,y1) (x2,y2)
                        | otherwise = Este: caminho (x1+1,y1) (x2,y2)
--46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) = case h of
  Norte -> vertical t
  Sul   -> vertical t
  _     -> False
--47
data Posicao = Pos Int Int deriving Show
maisCentral :: [Posicao] -> Posicao
maisCentral [] = error"Lista Vazia"
maisCentral [x] = x
maisCentral ((Pos x1 y1):(Pos x2 y2):t) | (x1^2 + y1^2) >= (x1^2 + y2^2) = maisCentral ((Pos x2 y2):t)
                                        | otherwise = maisCentral (Pos x1 y1:t)
--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x1 y1) ((Pos x2 y2):t)
 | (x2 == x1 -1) && (y1 == y2) || (x2 == x1 +1) && (y1 == y2) || (y2 == y1 +1) && (x1 == x2) || (y2 == y1 -1) && (x1 == x2) = (Pos x2 y2): vizinhos (Pos x1 y1) t
 | otherwise = vizinhos (Pos x1 y1) t
 --49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada []  = True
mesmaOrdenada [_] = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2):t) | y1 == y2 = mesmaOrdenada ((Pos x2 y2):t)
                                          | otherwise = False
--50
data Semaforo = Verde | Amarelo | Vermelho deriving Show
interseccaoOk :: [Semaforo] -> Bool
interseccaoOk [] = True
interseccaoOk (Vermelho:t) = interseccaoOk t
interseccaoOk (_:t)        = auxiliar t
 where
 auxiliar [] = True
 auxiliar (Vermelho:t) = auxiliar t
 auxiliar (_:t) = False
