module Ficha3 where

data Hora = H Int Int
     deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]
--1
valida2 :: Hora -> Bool
valida2 (H h m) = if (h >= 0 && h <= 23 && m >= 0 && m <= 59) then True else False
depois' :: Hora -> Hora -> Bool
depois' (H h1 m1) (H h2 m2) = if(h1 > h2) then True else if (m1 > m2 && h1 == h2) then True else False
horaparamin' :: Hora -> Int
horaparamin' (H h m) = h * 60 + m 
minutosparahora' :: Int -> Hora
minutosparahora' m = (H hora minuto)
 where
 hora = div m 60
 minuto = mod m 60
diffhoras' :: Hora -> Hora -> Int
diffhoras' (H h1 m1) (H h2 m2) = horaparamin' (H diffhora' diffmin')
 where
 diffhora' = abs(h2 - h1)
 diffmin' = abs(m2 -m1)
addMPH' :: Int -> Hora -> Hora
addMPH' aMais (H h m) = minutosparahora' horatudo
 where
 minutosTotal = aMais + m
 horaemmin = horaparamin'(H h 0)
 horatudo = minutosTotal + horaemmin

--a)
etapaValida :: Etapa -> Bool
etapaValida (h1, h2) = valida2 h1 && valida2 h2 && (depois' h2 h1)
--b)
viagemValida :: Viagem -> Bool
viagemValida [] = True
viagemValida [e] = etapaValida e
viagemValida ((c1,f1):(c2,f2):t) = etapaValida(c1,f1) && etapaValida(f1,c2) && viagemValida((c2,f2):t)
--c)
partidaChegada :: Viagem -> Etapa
partidaChegada [] = error"Viagem Inválida"
partidaChegada [e] = e
partidaChegada x = (fst(head x), snd(last x))
--d)
tempoEfetivo :: Viagem -> Hora
tempoEfetivo [(a,b)] = minutosparahora' (diffhoras' a b)
tempoEfetivo ((a,b):t) = addMPH' (diffhoras' a b) (tempoEfetivo t)
--e)
tempoEspera :: Viagem -> Hora
tempoEspera [(a,b)] = (H 0 0)
tempoEspera ((a,b):(c,d):t) = addMPH' (diffhoras' b c) (tempoEspera t)
--f)
tempoTotal :: Viagem -> Hora
tempoTotal v = minutosparahora' (horaparamin' total) 
 where
 total = somaHoras (tempoEspera v) (tempoEfetivo v)
 somaHoras (H a1 b1) (H a2 b2) = (H (a1+a2) (b1+b2))
--2
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
type Poligonal = [Ponto]
data Figura = Circulo Ponto Double
 | Rectangulo Ponto Ponto
 | Triangulo Ponto Ponto Ponto
 deriving (Show,Eq)
dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x1-x2)^2+(y1-y2)^2)
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan(x/y)
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
posx :: Ponto -> Double
posx (Cartesiano x y) = x
posy :: Ponto -> Double
posy (Cartesiano x y) = y
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2+y^2)
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

--a)
comprimento :: Poligonal -> Double
comprimento [p1] = 0
comprimento (p1:p2:t) = dist p1 p2 + comprimento (p2:t)
--b)
linhaFechada :: Poligonal ->Bool
linhaFechada [_,_] = False
linhaFechada [p1,p2,p3] = p1 == p3
linhaFechada (p1:p2:p3:t) = linhaFechada(p1:p3:t)
--c)
triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] = [Triangulo p1 p2 p3]
triangula (p1:p2:p3:t) = (Triangulo p1 p2 p3): triangula(p1:p3:t)
--d)
areaF :: Poligonal -> Double
areaF p = sum(map(\fig -> area'' fig) (triangula p))
--e)
mover :: Poligonal -> Ponto -> Poligonal
mover pol p = p:pol
--f)
zoom :: Double -> Poligonal -> Poligonal
zoom z [p1@(Cartesiano x y),p2@(Cartesiano a b)] = [p1,p3]
 where p3 = (Cartesiano (z*a) (z*b))
zoom z (p1@(Cartesiano x y):p2@(Cartesiano a b):pol) = p1:zoom z (p3:pol)
    where p3 = (Cartesiano (z*a) (z*b))
--3
data Contacto = Casa Integer
 | Trab Integer
 | Tlm Integer
 | Email String
 deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]
--a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome mail ((n,c):t)
 | nome == n = (n, aux mail c):t
 | otherwise = (n,c): acrescEmail nome mail t
 where
 aux mail [] = [Email mail]
 aux mail (Email m:t)
  | mail == m = (Email m):t
  | otherwise = (Email m): aux mail t
 aux mail (c:t) = c: aux mail t
--b)
verEmails :: Nome -> Agenda -> Maybe [String] 
verEmails _ [] = Nothing
verEmails nome ((n,c):t)
 | nome == n = Just (aux c)
 | otherwise = verEmails nome t
 where
 aux [] = []
 aux (Email m:t) = m: aux t
 aux (_:t) = aux t
--c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (c:t) = case c of
 Casa x -> x: a
 Trab x -> x: a
 Tlm x  -> x: a
 _      -> a
 where
 a = consTelefs t
--d)
casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome ((n,c):t)
 | nome == n = aux c
 | otherwise = casa nome t
 where
 aux [] = Nothing
 aux (Casa x:_) = Just x
 aux (_:t) = aux t
--4
type Dia = Int
type Mes = Int
type Ano = Int
--data Data = D Dia Mes Ano
-- deriving Show
type TabDN = [(Nome,Data)]
--a)
procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((n,d):t)
 | nome == n = Just d
 | otherwise = procura nome t
--b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D d1 m1 a1) nome ((n,D d2 m2 a2):t)
 | nome == n = if a1 > a2 && m1 > m2 || a1 > a2 && m1 >= m2 && d1 >= d2 then Just (b) else Just(b-1)  
 | otherwise = idade (D d1 m1 a2) nome t
 where
 b = a1 - a2
--c)
anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2) = a1 < a2 || a1 == a2 && m1 < m2 || a1 == a2 && m1 == m2 && d1 < d2
--d)
ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):ts) = insere (n,d) (ordena ts)
    where insere (n,d) [] = [(n,d)]
          insere (n,d) ((nh,dh):t) | anterior dh d = (nh,dh):insere (n,d) t
                                   | otherwise = (n,d):(nh,dh):t
--e)
porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade datans@(D d1 m1 a1) tabela = (n,idade): porIdade datans t
 where
 ((n,D d2 m2 a2):t) = ordena tabela
 idade = if a1 > a2 && m1 > m2 || a1 > a2 && m1 >= m2 && d1 >= d2 then b else b-1
 b = a1 - a2
--5
data Movimento = Credito Float | Debito Float
 deriving Show
data Data = D Int Int Int
 deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
 deriving Show
--a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext x ((_,_,m):t)) y = case m of
 Credito j -> if j > y then m:extValor (Ext x t) y else extValor (Ext x t) y
 Debito  j -> if j > y then m:extValor (Ext x t) y else extValor (Ext x t) y
--b)
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext x ((dat,des,mov):ls)) str = if elem des str then (dat,mov): filtro (Ext x ls) str else filtro (Ext x ls) str
--c)
creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext x ((_,_,mov):t)) = case mov of
 Credito j -> (j+a,b)
 Debito j  -> (a,j+b)
 where
 (a,b) = creDeb (Ext x t)
--d)
saldo :: Extracto -> Float
saldo (Ext x l) = foldl(\acc (_,_,mov) -> case mov of Credito b -> (acc + b); Debito b -> (acc - b)) x l






















