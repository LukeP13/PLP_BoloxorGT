module Bloc where
  import Posicio

  -- Tipus Dimensions
  data Dimensions3D = Dimensions3D Int Int Int -- dimensions x, y, z
                    deriving (Show, Eq)

  -- Retorna x, y o z de les Dimensions3D depenent del caràcter entrat
  get :: Dimensions3D -> Char -> Int
  get (Dimensions3D x y z) c
    | c == 'x' = x
    | c == 'y' = y
    | c == 'z' = z

  -- Retorna el valor X de les Dimensions3D
  getX :: Dimensions3D -> Int
  getX dim = get dim 'x'

  -- Retorna el valor Y de les Dimensions3D
  getY :: Dimensions3D -> Int
  getY dim = get dim 'y'

  -- Retorna el valor Z de les Dimensions3D
  getZ :: Dimensions3D -> Int
  getZ dim = get dim 'z'

  -- Retorna l'area de base X i altura Y
  areaXY :: Dimensions3D -> Int
  areaXY (Dimensions3D x y _) = x * y

  -- Tipus Bloc
  data Bloc = Bloc Posicio Dimensions3D
              deriving (Show, Eq)

  -- Crea un bloc de dimensions X = 1, Y = 1 i un determinat gruix Z i amb una posició inicial
  creaBloc :: Int -> Posicio -> Bloc -- Donat un gruix
  creaBloc g pos = Bloc pos dim
       where dim = Dimensions3D 1 1 g -- X i Y són iguals a 1

  -- Retorna la posició actual del bloc
  getPosBloc :: Bloc -> Posicio
  getPosBloc (Bloc pos dim) = pos

  -- Retorna les dimensions actuals del bloc
  getDimBloc :: Bloc -> Dimensions3D
  getDimBloc (Bloc pos dim) = dim

  -- Retorna en forma de llista totes les posicions que ocupen l'areaXY del bloc al tauler (les posicions que el bloc està "trepitjant")
  posBloc :: Bloc -> [Posicio]
  posBloc (Bloc posIni (Dimensions3D dx dy _)) = posList posIni posFi
       where posFi = augPos posIni (dx-1) (dy-1)
