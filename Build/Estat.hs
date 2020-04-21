module Estat where
  import Bloc
  import Tauler
  import Posicio
  import Data.List
  import Data.Char
  import System.IO()

  -- Genèrics
  updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
  updateMatrix m x (r,c) =
    take r m ++ [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m

  data Estat = Estat Bloc Tauler
  instance Show Estat where
    show (Estat b t) = show b ++ "\n" ++ show (dibuixaBloc b t)


  sortida :: [String] -> Estat
  sortida [] = error "Configuració buida"
  sortida (g:x:y:caselles) = Estat bloc tauler
          where tauler = creaTauler (read x) (read y) caselles
                bloc   = creaBloc (read g) $ head $ posSortida tauler


  resolt :: Estat -> Bool
  resolt (Estat b t) = isSubsetOf (posBloc b) (posGuanya t)

  dibuixaBloc :: Bloc -> Tauler -> Tauler
  dibuixaBloc b t = Tauler 0 0 (dibuixBloc l pl)
        where pl = posBloc b
              l = lines $ show t

  dibuixBloc :: [[Casella]] -> [Posicio] -> [[Casella]]
  dibuixBloc cll [] = cll
  dibuixBloc cll (Posicio x y:pl) = dibuixBloc (updateMatrix cll 'B' (x, y)) pl
