module Tauler where
  import Data.List
  import System.IO
  import Posicio

  type Tipus = Char

  data Casella = Casella Posicio Tipus
