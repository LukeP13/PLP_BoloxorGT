module Moviment where -- Modul que conté el tipus Moviment i les seves funcions
  -- Imports
  import Bloc
  import Posicio
  import Tauler
  -- Tipus Moviment
  data Moviment = Amunt | Avall | Dreta | Esquerra | Invalid
                deriving Show

  modificaPos :: Moviment -> Bloc -> Posicio
  modificaPos Amunt b = augPos (getPosBloc b) 0 (-(getZ (getDimBloc b)))
  modificaPos Avall b = augPos (getPosBloc b) 0 (getY (getDimBloc b))
  modificaPos Dreta b = augPos (getPosBloc b) (getX (getDimBloc b)) 0
  modificaPos Esquerra b = augPos (getPosBloc b) (-(getZ (getDimBloc b))) 0

  modificaDim :: Moviment -> Bloc -> Dimensions3D
  modificaDim Amunt b = Dimensions3D (getX (getDimBloc b)) (getZ (getDimBloc b)) (getY (getDimBloc b))
  modificaDim Avall b = Dimensions3D (getX (getDimBloc b)) (getZ (getDimBloc b)) (getY (getDimBloc b))
  modificaDim Dreta b = Dimensions3D (getZ (getDimBloc b)) (getY (getDimBloc b)) (getX (getDimBloc b))
  modificaDim Esquerra b = Dimensions3D (getZ (getDimBloc b)) (getY (getDimBloc b)) (getX (getDimBloc b))

  mou :: Moviment -> Bloc -> Bloc
  mou m b = Bloc (modificaPos m b) (modificaDim m b)

  totesPlenes :: Tauler -> [Posicio] -> Bool
  totesPlenes t (p:pl) = not(casellaBuida t p) && (totesPlenes t pl)

  comprovarCaselles :: Tauler -> Bloc -> Bool
  comprovarCaselles t b = totesPlenes t (posBloc b)

  esLegal :: Moviment -> Tauler -> Bloc -> Bool
  esLegal Amunt t b = comprovarCaselles t (mou Amunt b)
  esLegal Avall t b = comprovarCaselles t (mou Avall b)
  esLegal Dreta t b = comprovarCaselles t (mou Dreta b)
  esLegal Esquerra t b = comprovarCaselles t (mou Esquerra b)

  filtrar :: [Moviment] -> Tauler -> Bloc -> [Moviment]
  filtrar (m:ml) t b
      | esLegal m t b = m : filtrar ml t b
      | otherwise = filtrar ml t b

  legals :: Tauler -> Bloc -> [Moviment]
  legals t b = filtrar [Amunt, Avall, Dreta, Esquerra] t b

  creaMoviment :: Char -> Moviment
  creaMoviment m
      | m == 'w' = Amunt
      | m == 'a' = Esquerra
      | m == 's' = Avall
      | m == 'd' = Dreta
      | otherwise = Invalid
