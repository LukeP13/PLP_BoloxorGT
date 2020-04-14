module Bloc where
  import Data.List
  import System.IO
  import Posicio

  -- Tipus Dimensions
  data Dimensions = Dimensions Int Int Int -- dimensions x, y, z
                    deriving Show

  get :: Dimensions -> Char -> Int
  get (Dimensions x y z) c
    | c == 'x' = x
    | c == 'y' = y
    | c == 'z' = z

  getX :: Dimensions -> Int
  getX dim = get dim 'x'

  getY :: Dimensions -> Int
  getY dim = get dim 'y'

  getZ :: Dimensions -> Int
  getZ dim = get dim 'z'

  areaXY :: Dimensions -> Int
  areaXY (Dimensions x y _) = x * y

  -- Tipus Bloc
  data Bloc = Bloc Posicio Dimensions
              deriving Show

  posBloc :: Bloc -> [Posicio]
  posBloc (Bloc pos dim)
        | (superficie dim) > 1 = pos : posSeg : [] -- Com a màxim el bloc pot ocupar 2 posicions
        | otherwise = pos : []
        where superficie = areaXY
              posSeg = if (getX dim) > 1 then (augX pos) else (augY pos)

  
