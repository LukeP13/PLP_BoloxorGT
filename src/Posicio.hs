module Posicio where
  import Data.List
  import System.IO
  data Posicio = Posicio Int Int -- posicio respecte eixos x i y
                deriving Show

  aug :: Posicio -> Char -> Posicio
  aug (Posicio x y) c
    | c == 'x' = Posicio (x+1) y
    | c == 'y' = Posicio x (y+1)

  augX :: Posicio -> Posicio
  augX pos = aug pos 'x'

  augY :: Posicio -> Posicio
  augY pos = aug pos 'y'
