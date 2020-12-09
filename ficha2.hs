module Ficha2 where
import Data.Char

--2
--a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t
--b)
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre x (h:t) | x == h = 1 + numOcorre x t
                  | otherwise = numOcorre x t
--c
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) | h < 0 = False
                | otherwise = positivos t
--d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h <= 0 = soPos t
            | otherwise = h : soPos t
--e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + somaNeg t
              | otherwise = somaNeg t
--f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) = if length (h:t) <= 3 then (h:t) else tresUlt t
--g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y: segundos t
--h)
nosPrimeiros:: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((h,_):t)
    | a == h = True
    | otherwise = nosPrimeiros a t
--i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x+a,y+b,z+c)
 where
 (a,b,c) = sumTriplos t
--3
--a)
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if (h > '0' && h < '9') then h: soDigitos t else soDigitos t
--b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if isLower h then 1 + minusculas t else minusculas t
--c)
nums :: String -> [Int]
nums "" = []
nums (h:t) = if isDigit h then digitToInt h : nums t else nums t
--4
type Polinomio = [Monomio]
type Monomio = (Float,Int)
-- [(2,3), (3,4), (5,3), (4,5)] representa o polinómio  2 x ^ 3 + 3 x ^ 4 + 5 x ^ 3 + 4 x ^ 5   

--a)
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta x ((a,b):t) = if x == b then 1 + conta x t else conta x t
--b)
grau::Polinomio->Int
grau [(_,e)] = e
grau ((c1,e1):(c2,e2):t)
    | e1>= e2 = grau((c1,e1):t)
    | otherwise = grau((c2,e2):t)

--c)
selgrau::Int->Polinomio->Polinomio
selgrau _ [] = []
selgrau n ((c,e):t)
    | n == e = (c,e) : selgrau n t
    | otherwise = selgrau n t
--d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,e):xs) = if c == 0 then deriv xs else (c*(fromIntegral e),e-1) : deriv xs
--e)
calcula _ [] = 0
calcula a ((x,y):t)=x*(a^y)+calcula a t
--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t) = if x == 0 then simp t else (x,y):simp t
--g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,b) ((x,y):t) = (a*x,b+y): mult (a,b) t
--h)
normaliza:: Polinomio->Polinomio
normaliza [] =[]
normaliza [(x,y)] = [(x,y)]
normaliza ((x1,y1):(x2,y2):t)
  | y1 == y2 = normaliza ((x1 + x2, y1):t)
  | conta y1 t == 0 = (x1, y1):normaliza((x2, y2):t)
  | otherwise = normaliza ((x1, y1):t ++ [(x2, y2)])

--i)
soma::Polinomio->Polinomio->Polinomio
soma p1 p2 = normaliza (p1++p2)
--j)
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto (h:t) l2 = mult h l2 ++ produto t l2 
--k)
ordena:: Polinomio->Polinomio
ordena [] = []
ordena ((c,e):t) = (auxOrd (c,e) (ordena t))

auxOrd::Monomio->Polinomio->Polinomio
auxOrd (c,e) [] = [(c,e)]
auxOrd (c1,e1) ((c2,e2):t)
    | e1 <= e2 = ((c1,e1): (c2,e2): t)
    | otherwise = (c2,e2): auxOrd (c1,e1) t
--l)
equiv:: Polinomio->Polinomio->Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)


