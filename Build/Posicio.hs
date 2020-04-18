module Posicio where -- Modul que conté el tipus Posicio i les seves funcions
  -- Imports
  import Data.List()
  import System.IO()

  -- Tipus Posicio
  data Posicio = Posicio Int Int -- Posicio respecte eixos x i y
                deriving Show

  -- Donades una Posicio i un nombre de files i columnes, dona la posicio final
  augPos :: Posicio -> Int -> Int -> Posicio
  augPos (Posicio px py) augx augy = Posicio (px+augx) (py+augy)

  -- Crea una llista de Posicions desde la inicial fins la final
  posList :: Posicio -> Posicio -> [Posicio] -- Posicions inici i fi
  posList (Posicio ix iy) (Posicio fx fy) = [Posicio x y | x <- [ix..fx], y <- [iy..fy]]
