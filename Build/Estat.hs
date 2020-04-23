module Estat where
  import Bloc
  import Tauler
  import Posicio
  import Moviment

  -- Genèrics
  updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
  updateMatrix m x (r,c) =
    take r m ++ [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m

  data Estat = Estat Bloc Tauler
  instance Show Estat where
    show (Estat b t) = show (dibuixaBloc b t)


  sortida :: [String] -> Estat
  sortida [] = error "Configuració buida"
  sortida (g:x:y:caselles) = Estat bloc tauler
          where tauler = creaTauler (read x) (read y) caselles
                bloc   = creaBloc (read g) $ head $ posSortida tauler

  mostraMon :: Estat -> String
  mostraMon (Estat b t) = show t

  resolt :: Estat -> Bool
  resolt (Estat b t) = isSubsetOf (posBloc b) (posGuanya t)

  fora :: Estat -> Moviment -> Bool
  fora (Estat b t) m = False

  dibuixaBloc :: Bloc -> Tauler -> Tauler
  dibuixaBloc b t = Tauler 0 0 (dibuixBloc l pl)
        where pl = posBloc b
              l = lines $ showRealT t

  dibuixBloc :: [[Casella]] -> [Posicio] -> [[Casella]]
  dibuixBloc cll [] = cll
  dibuixBloc cll (Posicio x y:pl) = dibuixBloc (updateMatrix cll 'B' (x, y)) pl

  execMovim :: Estat -> Moviment -> Estat
  execMovim (Estat b t) m = (Estat (mou m b) t )
