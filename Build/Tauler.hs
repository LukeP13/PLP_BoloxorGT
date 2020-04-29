module Tauler where
  import Data.List()
  import Data.Tuple()
  import System.IO()
  import Posicio

  -- Mètodes Generics

  -- Retorna una llista de tuples formada pels elements de [a] (claus) i [b] (valors)
  listOfTuples :: [a] -> [b] -> [(a,b)]
  listOfTuples = zipWith (\a b -> (a, b))

  -- Retorna els index en els quals apareix un element
  indCount :: Eq a => [a] -> a -> [Int]
  indCount str ch = [ y | (x, y) <- zip str [0..], x == ch]

  -- Retorna les posicions en les que hi ha un element
  posCount :: Eq a => Int -> [[a]] -> a -> [Posicio]
  posCount _ [] _ = []
  posCount i (x:xs) typ = [Posicio i y | y <- indCount x typ] ++ posCount (i+1) xs typ

  -- Retorna cert si l'element entrat es troba entre els valors de la tupla (més gran que la clau i més petit que el valor)
  betw :: Ord a => a -> (a, a) -> Bool
  x `betw` (y, z) = y <= x && x <= z

  -- Retorna cert si la primera llista és un subconjunt de la segona
  isSubsetOf :: Eq a => [a] -> [a] -> Bool
  isSubsetOf [] _ = True
  isSubsetOf _ [] = False
  isSubsetOf (x:xs) y = x `elem` y && isSubsetOf xs y

  -- Inverteix la matriu
  invert :: [[a]] -> [[a]]
  invert [] = []
  invert ([]:_) = []
  invert m = [[m !! x !! y | x <- [0..length m-1]] | y <- [0..length (head m)-1]]

  -- Tipus Casella
  type Casella = Char -- Posicio, Tipus
  show' :: Casella -> String
  show' c = [c]

  -- Mostra les caselles de la llista
  mostraCaselles :: [Casella] -> String
  mostraCaselles = foldr((++) . show') []

  -- Retorna cert si la casella es buida ('0') o fals altrament
  esBuida :: Casella -> Bool
  esBuida c = c == '0'

  -- Retorna les posicions en les que hi ha un tipus determinat de caselles al tauler
  posTipus :: Tauler -> Char -> [Posicio]
  posTipus (Tauler _ _ []) _ = []
  posTipus (Tauler _ _ cll) tipus = posCount 0 cll tipus


  -- Tipus Tauler
  data Tauler = Tauler Int Int [[Casella]] -- x, y, caselles
                deriving Eq
  instance Show Tauler where
    show (Tauler _ _ t) = mostraTauler $ invert t -- Inverteix la matriu de caselles per una correcta visualització

  -- Crea un tauler a partir de les dimensions x, y i el tipus de cada casella
  -- Inverteix la matriu ja que fitxer es llegeix invertit
  creaTauler :: Int -> Int -> [[Casella]] -> Tauler -- x, y, tipus de cada casella
  creaTauler x y cll = Tauler y x (invert cll)

  -- Retorna cert si una posició del tauler pertany a una casella buida o fals altrament
  casellaBuida :: Tauler -> Posicio -> Bool
  casellaBuida (Tauler tx ty cll) (Posicio px py)
      | px `betw` (0, tx-1) && py `betw` (0, ty-1) = esBuida (cll !! px !! py) -- Sí està dins del rang del tauler
      | otherwise = True  -- Si no està dins el tauler és buida sempre

  -- Retorna les posicions que ocupen la sortida
  posSortida :: Tauler -> [Posicio]
  posSortida t = posTipus t 'S'

  -- Retorna les posicions que ocupen la meta
  posGuanya :: Tauler -> [Posicio]
  posGuanya t = posTipus t 'G'

  -- Mostra una matriu de caselles per pantalla
  mostraTauler :: [[Casella]] -> String
  mostraTauler [] = []
  mostraTauler (cl:cll) = mostraCaselles cl ++ "\n" ++ mostraTauler cll

  -- Mostra el tauler per pantalla
  showRealT :: Tauler -> String
  showRealT (Tauler _ _ cll) = mostraTauler cll
