module Moviment where -- Modul que conté el tipus Moviment i les seves funcions
  -- Imports
  import Bloc
  import Posicio
  import Tauler
  -- Tipus Moviment
  data Moviment = Amunt | Avall | Dreta | Esquerra | Invalid
                deriving (Show, Eq, Enum, Ord)



  modificaPos :: Moviment -> Bloc -> Posicio
  modificaPos m (Bloc pos (Dimensions3D dx dy dz))
    | m == Amunt = novaPos 0 (-dz)
    | m == Avall = novaPos 0 dy
    | m == Dreta = novaPos dx 0
    | m == Esquerra = novaPos (-dz) 0
    | otherwise = error "Posicio invàlida"
    where novaPos = augPos pos


  modificaDim :: Moviment -> Bloc -> Dimensions3D
  modificaDim m (Bloc p (Dimensions3D x y z))
    | m <= Avall    = Dimensions3D x z y
    | m <= Esquerra = Dimensions3D z y x
    | otherwise = error "Moviment invàlid"

  mou :: Moviment -> Bloc -> Bloc
  mou m b = Bloc (modificaPos m b) (modificaDim m b)

  totesPlenes :: Tauler -> [Posicio] -> Bool
  totesPlenes _ [] = True
  totesPlenes t (p:pl) = not(casellaBuida t p) && totesPlenes t pl

  comprovarCaselles :: Tauler -> Bloc -> Bool
  comprovarCaselles t b = totesPlenes t (posBloc b)

  esLegal :: Moviment -> Tauler -> Bloc -> Bool
  esLegal Invalid _ _ = False
  esLegal m t b = comprovarCaselles t (mou m b)

  filtrar :: [Moviment] -> Tauler -> Bloc -> [Moviment]
  filtrar [] _ _ = []
  filtrar (m:ml) t b
      | esLegal m t b = m : filtrar ml t b
      | otherwise = filtrar ml t b

  legals :: Tauler -> Bloc -> [Moviment]
  legals = filtrar [Amunt .. Esquerra]

  creaMoviment :: Char -> Moviment
  creaMoviment m
      | m == 'w' = Amunt
      | m == 'a' = Esquerra
      | m == 's' = Avall
      | m == 'd' = Dreta
      | otherwise = Invalid
