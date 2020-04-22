module Tauler where
  import Posicio

  -- Mètodes Generics
  listOfTuples :: [a] -> [b] -> [(a,b)]
  listOfTuples = zipWith (\a b -> (a, b))

  indCount :: Eq a => [a] -> a -> [Int] -- Busca els index en els quals apareix un element
  indCount str ch = [ y | (x, y) <- zip str [0..], x == ch]

  posCount :: Eq a => Int -> [[a]] -> a -> [Posicio] -- Busca les posicions en les que hi ha un element
  posCount _ [] _ = []
  posCount i (x:xs) typ = [Posicio i y | y <- indCount x typ] ++ posCount (i+1) xs typ

  betw :: Ord a => a -> (a, a) -> Bool
  x `betw` (y, z) = y <= x && x <= z

  isSubsetOf :: Eq a => [a] -> [a] -> Bool
  isSubsetOf [] _ = True
  isSubsetOf _ [] = False
  isSubsetOf (x:xs) y = x `elem` y && isSubsetOf xs y

  -- Tipus Casella
  type Casella = Char -- Posicio, Tipus
  show' :: Casella -> String
  show' c = [c]

  mostraCaselles :: [Casella] -> String
  mostraCaselles = foldr ((++) . show') []

  esBuida :: Casella -> Bool
  esBuida c = c == '0'

  posTipus :: Tauler -> Char -> [Posicio]
  posTipus (Tauler _ _ []) _ = []
  posTipus (Tauler _ _ cll) tipus = posCount 0 cll tipus


  -- Tipus Tauler
  data Tauler = Tauler Int Int [[Casella]] -- nfiles, ncolumnes, caselles
  instance Show Tauler where
    show = mostraTauler

  creaTauler :: Int -> Int -> [[Char]] -> Tauler -- nfiles, ncolumnes, tipus de cada casella
  creaTauler = Tauler

  casellaBuida :: Tauler -> Posicio -> Bool
  casellaBuida (Tauler tx ty cll) (Posicio px py)
      | px `betw` (0, tx-1) && py `betw` (0, ty-1) = esBuida (cll !! px !! py) -- Sí està dins del rang del tauler
      | otherwise = True  -- Si no està dins el tauler és buida sempre

  posSortida :: Tauler -> [Posicio]
  posSortida t = posTipus t 'S'

  posGuanya :: Tauler -> [Posicio]
  posGuanya t = posTipus t 'G'

  mostraTauler :: Tauler -> String
  mostraTauler (Tauler _ _ []) = []
  mostraTauler (Tauler _ _ (cl:cll)) = mostraCaselles cl ++ "\n" ++ mostraTauler (Tauler 0 0 cll)
