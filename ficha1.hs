module Ficha1 where
import Data.Char

--1
--a)
perimetro1 :: Double -> Double
perimetro1 r = 2 * pi * r
--b)
dist' :: (Double, Double) -> (Double, Double) -> Double
dist' (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
--c)
primUlt :: [a] -> (a,a)
primUlt [] = error"Lista vazia"
primUlt l = (head l, last l)
--d)
multiplo1 :: Integer -> Integer -> Bool
multiplo1 a b = (mod a b) == 0
--e)
truncaImpar :: [a] -> [a]
truncaImpar l = if (mod (length l) 2) == 0 then l else tail l
--f)
max2 :: Integer -> Integer -> Integer
max2 a b = if a > b then a else b
--g)
max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = max2 a (max2 b c)
--2
--a)
nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c | det > 0 = 2
              | det == 0 = 1
              | otherwise = 0
 where
 det = b^2 - 4*a*c
--b)
raizes :: Double -> Double -> Double -> [Double]
raizes a b c | nRaizes a b c == 2 = [r1,r2]
             | nRaizes a b c == 1 = [-b/(2*a)]
             | otherwise = []
 where
 det = b^2 - 4*a*c
 r1 = (-b + det)/(2*a)
 r2 = (-b - det)/(2*a)
--3
type Hora1 = (Int,Int)
--a)
horaValida :: Hora1 -> Bool
horaValida (a,b) = (a < 24 && a >= 0) && (b < 60 && b >= 0)
--b)
horaDepois :: Hora1 -> Hora1 -> Bool
horaDepois (a1,b1) (a2,b2) = (a1 > a2) && (a1 == a2 && b1 > b2)
--c)
horaParaMin :: Hora1 -> Int
horaParaMin (a,b) = a * 60 + b
--d)
minParaHora :: Int -> Hora1
minParaHora x = (div x 60, mod x 60)
--e)
diffHoras :: Hora1 -> Hora1 -> Int
diffHoras (a1,b1) (a2,b2) = horaParaMin((abs(a1-a2),abs(b1-b2)))
--f)
adicionaMin :: Hora1 -> Int -> Hora1
adicionaMin (a,b) x = minParaHora(horaParaMin (a,b) + x)
--4
data Hora = H Int Int deriving (Show,Eq)
--a)
horaValida' :: Hora -> Bool
horaValida' (H a b) = (a < 24 && a >= 0) && (b < 60 && b >= 0)
--b)
horaDepois' :: Hora -> Hora -> Bool
horaDepois' (H a1 b1) (H a2 b2) = (a1 > a2) && (a1 == a2 && b1 > b2)
--c)
horaParaMin' :: Hora -> Int
horaParaMin' (H a b) = a * 60 + b
--d)
minParaHora' :: Int -> Hora
minParaHora' x = (H (div x 60) (mod x 60))
--e)
diffHoras' :: Hora -> Hora -> Int
diffHoras' (H a1 b1) (H a2 b2) = horaParaMin' (H (abs(a1-a2)) (abs(b1-b2)))
--f)
adicionaMin' :: Hora -> Int -> Hora
adicionaMin' (H a b) x = minParaHora' (horaParaMin' (H a b) + x)
--5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)
--a)
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde
--b)
stop :: Semaforo -> Bool
stop a = case a of
 Vermelho -> True
 _        -> False
--c)
safe :: Semaforo -> Semaforo -> Bool
safe a b = stop a || stop b
--6
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
paraC :: Ponto -> Ponto
paraC (Cartesiano x y) = (Polar raio angulo)
 where
  raio = dist (Cartesiano 0 0) (Cartesiano x y)
  angulo = acos(x / raio)
paraP :: Ponto -> Ponto
paraP (Polar d a) = (Cartesiano abcissa ordenada)
 where
  abcissa = cos(a) * d
  ordenada = sin(a) * d
--a)
posx :: Ponto -> Double
posx (Cartesiano x y) = x
--b)
posy :: Ponto -> Double
posy (Cartesiano x y) = y
--c)
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2+y^2)
--d)
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan(x/y)
--e)
dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x1-x2)^2+(y1-y2)^2)
--7
data Figura = Circulo Ponto Double
 | Rectangulo Ponto Ponto
 | Triangulo Ponto Ponto Ponto
 deriving (Show,Eq)
--a)
poligono :: Figura -> Bool
poligono x = case x of
 Circulo _ _ -> False
 _       -> True
--b)
vertices :: Figura -> [Ponto]
vertices f = case f of
  Triangulo p1 p2 p3 -> [p1,p2,p3]
  Rectangulo p1 p2 -> [p1,p2,p3,p4] where
    p3 = Cartesiano (posx p1) (posy p2)
    p4 = Cartesiano (posx p2) (posy p1)
  _ -> []
--c)
dist'' :: Ponto -> Ponto -> Double
dist'' (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
area'' :: Figura -> Double
area'' (Triangulo p1 p2 p3) =
 let a = dist'' p1 p2
     b = dist'' p2 p3
     c = dist'' p3 p1
     s = (a+b+c) / 2 -- semi-perimetro
 in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area'' (Circulo _ raio) = pi * raio * raio
area'' (Rectangulo p1 p2) = 
 let p3 = Cartesiano (posx p1) (posy p2)
     c = dist'' p1 p3
     l = dist'' p2 p3
 in c * l
--d)
perimetro :: Figura -> Double
perimetro (Circulo _ raio) = 2 * pi * raio
perimetro' (Rectangulo p1 p2) = 2 * l + 2 * c
 where
 p3 = Cartesiano (posx p1) (posy p2)
 l = dist p1 p3
 c = dist p2 p3
perimetro' (Triangulo p1 p2 p3) = a + b + c
 where
 a = dist p1 p2
 b = dist p2 p3
 c = dist p1 p3
--8
{-x
ord :: Char -> Int
chr :: Int -> Char 
-}
--a)
isLower' :: Char -> Bool
isLower' c = (ord c >= 97) && (ord c <= 122)
--b)
isDigit' :: Char -> Bool
isDigit' c = (ord c >= 48) && (ord c <= 57)
--c)
isAlpha' :: Char -> Bool
isAlpha' c = isLower' c || isUpper' c
--d)
isUpper' :: Char -> Bool
isUpper' c = (ord c >= 65) && (ord c <= 90)
--e)
intToDigit' :: Int -> Char
intToDigit' x = chr(x + 48)
--f)
digitToInt' :: Char -> Int
digitToInt' c = ord c - 48