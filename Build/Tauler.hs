module Tauler where
  import Data.List()
  import System.IO()
  import Posicio


  data Casella = Casella Posicio Char -- Posicio, Tipus
                 deriving Show

  data Tauler = Tauler Int Int [Casella] -- nfiles, ncolumnes, caselles
                deriving Show


  creaTauler :: Int -> Int -> [Char] -> Tauler -- nfiles, ncolumnes, llista de tipus
  creaTauler x y tList = Tauler x y [Casella (pList !! i) (tList !! i) | i <- [0 .. (length pList-1)]]
                  where pList = posList (Posicio 0 0) (Posicio (x-1) (y-1)) -- Llista de caselles de mida x y


  mostra :: Tauler -> IO()
  mostra (Tauler x y caselles) = do
    putStrLn (show caselles)
