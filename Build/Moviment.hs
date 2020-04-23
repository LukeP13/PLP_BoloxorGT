module Moviment where -- Modul que conté el tipus Moviment i les seves funcions
  -- Imports
  import Bloc
  import Posicio

  -- Tipus Moviment
  data Moviment = Amunt Bloc
                |Avall Bloc
                |Dreta Bloc
                |Esquerra Bloc
                deriving Show

  modificaPos :: Moviment -> Posicio
  modificaPos (Amunt b) = augPos (getPosBloc b) 0 (-1)
  modificaPos (Avall b) = augPos (getPosBloc b) 0 1
  modificaPos (Dreta b) = augPos (getPosBloc b) 1 0
  modificaPos (Esquerra b) = augPos (getPosBloc b) (-1) 0

  modificaDim :: Moviment -> Dimensions3D
  modificaDim (Amunt b) = Dimensions3D (getX (getDimBloc b)) (getZ (getDimBloc b)) (getY (getDimBloc b))
  modificaDim (Avall b) = Dimensions3D (getX (getDimBloc b)) (getZ (getDimBloc b)) (getY (getDimBloc b))
  modificaDim (Dreta b) = Dimensions3D (getZ (getDimBloc b)) (getY (getDimBloc b)) (getX (getDimBloc b))
  modificaDim (Esquerra b) = Dimensions3D (getZ (getDimBloc b)) (getY (getDimBloc b)) (getX (getDimBloc b))

  mou :: Moviment -> Bloc
  mou m = Bloc (modificaPos m) (modificaDim m)
