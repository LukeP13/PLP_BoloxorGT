module Bloc where
  import Data.List()
  import System.IO()
  import Posicio

  -- Tipus Dimensions
  data Dimensions3D = Dimensions3D Int Int Int -- dimensions x, y, z
                    deriving Show

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
              deriving Show

  posBloc :: Bloc -> [Posicio]
  posBloc (Bloc posIni (Dimensions3D dx dy _)) = posList pos posFi
        where posFi = augPos pos (dx-1) (d7-1)
