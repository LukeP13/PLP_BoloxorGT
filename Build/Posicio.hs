module Posicio where -- Modul que conté el tipus Posicio i les seves funcions
  -- Tipus Posicio
  data Posicio = Posicio Int Int -- Posicio respecte eixos x i y
                deriving (Show, Eq)

  -- Donades una Posicio i un nombre de files i columnes, dona la posicio final
  augPos :: Posicio -> Int -> Int -> Posicio
  augPos (Posicio px py) augx augy = Posicio (px+augx) (py+augy)

  -- Crea una llista de Posicions desde la inicial fins la final
  posList :: Posicio -> Posicio -> [Posicio] -- Posicions inici i fi
  posList (Posicio ix iy) (Posicio fx fy) = [Posicio x y | x <- [ix..fx], y <- [iy..fy]]

  -- Agrupa una llista en llistes mes petites de n elements
  group :: Int -> [a] -> [[a]]
  group _ [] = []
  group n l
    | n > 0 = (take n l) : (group n (drop n l))
    | otherwise = error "Negative or zero n"

  getPosX :: Posicio -> Int
  getPosX (Posicio x y) = x

  getPosY :: Posicio -> Int
  getPosY (Posicio x y) = y
