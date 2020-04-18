module Tauler where
  import Data.List()
  import System.IO()
  import Posicio


  data Casella = Casella Posicio Char -- Posicio, Tipus



  data Tauler = Tauler Int Int [Casella] -- nfiles, ncolumnes, caselles


  creaTauler :: Int -> Int -> Tauler
  creaTauler x y = Tauler 1 1 [(Casella 1 1)]
