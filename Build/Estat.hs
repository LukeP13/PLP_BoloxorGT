module Estat where
  import Bloc
  import Tauler
  import Posicio
  import Moviment

  -- Mètodes Genèrics

  -- Afegeix un element a la llista a una determinada posició
  updateList :: [a] -> a -> Int -> [a]
  updateList l val pos = take pos l ++ [val] ++ drop (pos + 1) l

  -- Afegeix un element a la matriu a una determinada posició
  updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
  updateMatrix m val (r,c) =
    take r m ++ [updateList (m !! r) val c] ++ drop (r + 1) m

  -- Tipus Estat
  data Estat = Estat Bloc Tauler | Nul
              deriving Eq
  instance Show Estat where
    show (Estat b t) = show (dibuixaBloc b t)

  --Crea un estat inicial
  sortida :: [String] -> Estat
  sortida [] = error "Configuració buida"
  sortida (g:x:y:caselles) = Estat bloc tauler
          where tauler = creaTauler (read x) (read y) caselles -- Es crea el tauler
                bloc   = creaBloc (read g) $ head $ posSortida tauler -- Es crea el bloc

  -- Mostra el tauler amb el bloc
  mostraMon :: Estat -> String
  mostraMon (Estat b t) = show t

  -- Retorna cert si l'estat actual del bloc i tauler correspon a l'estat en que es considera el joc com a resolt o fals altrament
  resolt :: Estat -> Bool
  resolt (Estat b t) = isSubsetOf (posBloc b) (posGuanya t)

  -- Retorna cert si al fer un moviment en l'estat el bloc cau fora del tauler o fals altrament
  fora :: Estat -> Moviment -> Bool
  fora (Estat b t) m = not $ esLegal m t b

  -- Dibuixa el bloc al tauler
  dibuixaBloc :: Bloc -> Tauler -> Tauler
  dibuixaBloc b t = Tauler 0 0 (dibuixBloc l pl) -- Es retorna un tauler amb el bloc dibuixat al seu conjunt de caselles
        where pl = posBloc b
              l = lines $ showRealT t

  -- Dibuixa el bloc (caràcter 'B') al conjunt de caselles
  dibuixBloc :: [[Casella]] -> [Posicio] -> [[Casella]]
  dibuixBloc cll [] = cll
  dibuixBloc cll (Posicio x y:pl) = dibuixBloc (updateMatrix cll 'B' (x, y)) pl

  -- Mou el bloc d'un estat sobre el tauler d'aquest estat
  execMovim :: Estat -> Moviment -> Estat
  execMovim (Estat b t) m = (Estat (mou m b) t)
