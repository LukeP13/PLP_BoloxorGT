module Tauler where
  import Data.List()
  import Data.Tuple()
  import System.IO()
  import Posicio

  -- Mètodes Genèrics
  listOfTuples :: [a] -> [b] -> [(a,b)]
  listOfTuples = zipWith (\a b -> (a, b))

  -- Tipus Casella
  data Casella = Casella Posicio Char -- Posicio, Tipus
  instance Show Casella where
    show (Casella _ tipus) = [tipus]

  mostraCaselles :: [Casella] -> String
  mostraCaselles = foldr ((++) . show) []

  listCaselles :: [Posicio] -> String -> [Casella]
  listCaselles pl tl
      | length pl == length tl = [Casella p t | (p, t) <- listOfTuples pl tl]
      | otherwise = error "Llista de posicions i tipus no concorden en mida"


  -- Tipus Tauler
  data Tauler = Tauler Int Int [[Casella]] -- nfiles, ncolumnes, caselles
  instance Show Tauler where
    show = mostraTauler

  creaTauler :: Int -> Int -> [String] -> Tauler -- nfiles, ncolumnes, tipus de cada casella
  creaTauler x y tll = Tauler x y [listCaselles pl tl | (pl, tl) <- listOfTuples pll tll]
                 where pll = [posList (Posicio i 0) (Posicio i (y-1)) | i <- [0..(x-1)]] -- Llista de caselles de mida x y


  mostraTauler :: Tauler -> String
  mostraTauler (Tauler _ _ []) = []
  mostraTauler (Tauler _ _ (c:cl)) = mostraCaselles c ++ "\n" ++ mostraTauler (Tauler 0 0 cl)
