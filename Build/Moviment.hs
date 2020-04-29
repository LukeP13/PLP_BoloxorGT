module Moviment where -- Modul que conté el tipus Moviment i les seves funcions
  -- Imports
  import Bloc
  import Posicio
  import Tauler

  -- Tipus Moviment
  data Moviment = Amunt | Avall | Dreta | Esquerra | Invalid
                deriving (Show, Eq, Enum, Ord)


  -- Modifica la posició actual del bloc
  modificaPos :: Moviment -> Bloc -> Posicio
  modificaPos m (Bloc pos (Dimensions3D dx dy dz)) -- Aquesta posició sempre apunta al cantó superior esquerre del bloc:
    | m == Amunt = novaPos 0 (-dz) -- Reduïm la posició Y segons la dimensió Z del bloc
    | m == Avall = novaPos 0 dy -- Augmentem posició Y segons la dimensió Y del bloc
    | m == Dreta = novaPos dx 0 -- Augmentem posició X segons la dimensió X del bloc
    | m == Esquerra = novaPos (-dz) 0 -- Reduïm la posició X segons la dimensió Z del bloc
    | otherwise = error "Posicio invàlida"
    where novaPos = augPos pos


  -- Modifica les dimensions actuals del bloc
  modificaDim :: Moviment -> Bloc -> Dimensions3D
  modificaDim m (Bloc p (Dimensions3D x y z))
    | m <= Avall    = Dimensions3D x z y -- Si anem amunt o avall intercanviem Y per Z
    | m <= Esquerra = Dimensions3D z y x -- Si anem a la dreta o a l'esquerra intercanviem X per Z
    | otherwise = error "Moviment invàlid"

  -- Mou el bloc a la direcció desitjada modificant la seva posició i dimensions
  mou :: Moviment -> Bloc -> Bloc
  mou m b = Bloc (modificaPos m b) (modificaDim m b)

  -- Retorna cert si totes les posicions de la llista son caselles plenes al tauler o fals altrament
  totesPlenes :: Tauler -> [Posicio] -> Bool
  totesPlenes _ [] = True
  totesPlenes t (p:pl) = not(casellaBuida t p) && totesPlenes t pl

  -- Retorna cert si totes les "futures" caselles que "trepitja" el bloc al efectuar el moviment son caselles plenes al tauler o fals altrament
  comprovarCaselles :: Tauler -> Bloc -> Bool
  comprovarCaselles t b = totesPlenes t (posBloc b)

  -- Retorna cert si un moviment desitjat és legal (totes les caselles "trepitjades" son plenes)
  esLegal :: Moviment -> Tauler -> Bloc -> Bool
  esLegal Invalid _ _ = False
  esLegal m t b = comprovarCaselles t (mou m b) -- Comprovem les "futures" caselles "trepitjades"

  -- Treu de la llista de moviments entrada els moviments ilegals (sobre un tauler amb un bloc) i retorna els legals
  filtrar :: [Moviment] -> Tauler -> Bloc -> [Moviment]
  filtrar [] _ _ = []
  filtrar (m:ml) t b -- Comprovem cada moviment de la llista
      | esLegal m t b = m : filtrar ml t b
      | otherwise = filtrar ml t b

  -- Retorna els moviments legals que pot fer el bloc a la posició actual al tauler
  legals :: Tauler -> Bloc -> [Moviment]
  legals = filtrar [Amunt .. Esquerra]

  -- Retorna un moviment diferent depenent del caràcter entrat
  creaMoviment :: Char -> Moviment
  creaMoviment m
      | m == 'w' = Amunt
      | m == 'a' = Esquerra
      | m == 's' = Avall
      | m == 'd' = Dreta
      | otherwise = Invalid -- Si no es cap dels anteriors és un moviment invàlid
