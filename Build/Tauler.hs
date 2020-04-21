module Tauler where
  import Data.List()
  import Data.Tuple()
  import System.IO()
  import Posicio

  -- Mètodes Genèrics
  listOfTuples :: [a] -> [b] -> [(a,b)]
  listOfTuples = zipWith (\a b -> (a, b))

  betw :: Ord a => a -> (a, a) -> Bool
  x `betw` (y, z) = y <= x && x <= z

  -- Tipus Casella
  type Casella = Char -- Posicio, Tipus
  show' :: Casella -> String
  show' c = [' ', c]

  mostraCaselles :: [Casella] -> String
  mostraCaselles = foldr ((++) . show') []

  esBuida :: Casella -> Bool
  esBuida c = c == '0'

  -- Tipus Tauler
  data Tauler = Tauler Int Int [[Casella]] -- nfiles, ncolumnes, caselles
  instance Show Tauler where
    show = mostraTauler

  creaTauler :: Int -> Int -> [[Casella]] -> Tauler -- nfiles, ncolumnes, tipus de cada casella
  creaTauler x y tll = Tauler x y tll

  casellaBuida :: Tauler -> Posicio -> Bool
  casellaBuida (Tauler tx ty cll) (Posicio px py)
      | px `betw` (0, tx-1) && py `betw` (0, ty-1) = esBuida (cll !! px !! py)
      | otherwise = True -- Si no està dins el tauler és buida


  mostraTauler :: Tauler -> String
  mostraTauler (Tauler _ _ []) = []
  mostraTauler (Tauler _ _ (c:cl)) = mostraCaselles c ++ "\n" ++ mostraTauler (Tauler 0 0 cl)
