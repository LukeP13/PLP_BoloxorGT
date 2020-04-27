module Bloc where
  import Posicio

  -- Tipus Dimensions
  data Dimensions3D = Dimensions3D Int Int Int -- dimensions x, y, z
                    deriving (Show, Eq)

  get :: Dimensions3D -> Char -> Int
  get (Dimensions3D x y z) c
    | c == 'x' = x
    | c == 'y' = y
    | c == 'z' = z

  getX :: Dimensions3D -> Int
  getX dim = get dim 'x'

  getY :: Dimensions3D -> Int
  getY dim = get dim 'y'

  getZ :: Dimensions3D -> Int
  getZ dim = get dim 'z'

  areaXY :: Dimensions3D -> Int
  areaXY (Dimensions3D x y _) = x * y

  -- Tipus Bloc
  data Bloc = Bloc Posicio Dimensions3D
              deriving (Show, Eq)

  creaBloc :: Int -> Posicio -> Bloc -- Donat un gruix
  creaBloc g pos = Bloc pos dim
       where dim = Dimensions3D 1 1 g

  getPosBloc :: Bloc -> Posicio
  getPosBloc (Bloc pos dim) = pos

  getDimBloc :: Bloc -> Dimensions3D
  getDimBloc (Bloc pos dim) = dim

  posBloc :: Bloc -> [Posicio]
  posBloc (Bloc posIni (Dimensions3D dx dy _)) = posList posIni posFi
       where posFi = augPos posIni (dx-1) (dy-1)
