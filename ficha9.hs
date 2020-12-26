import System.Random
--1
--randomIO :: Random a => IO a
--randomRIO :: Random a => (a,a) -> IO a
--a)
bingo :: IO ()
bingo = do novaLista <- acumularNumeros []
           getChar
           mostra novaLista


acumularNumeros :: [Int] -> IO [Int]
acumularNumeros l | length l == 90 = do return l
                  | otherwise = do v <- randomRIO (1,90)
                                   let novaLista = if v `elem` l then l else v:l in acumularNumeros novaLista

mostra :: [Int] -> IO ()
mostra [] = do putStrLn "Fim do Bingo"
mostra (x:xs) = do print x
                   getChar
                   mostra xs

--b)
{-
secreta :: IO (Int, Int, Int, Int)
secreta = do 
 alpha <- randomRIO (0,9)
 beta <- randomRIO (0,9)
 chi <- randomRIO (0,9)
 delta <- randomRIO (0,9)
 return (alpha, beta, chi, delta)

mastermind :: IO ()
mastermind = do (a,b,c,d) <- secreta
-}