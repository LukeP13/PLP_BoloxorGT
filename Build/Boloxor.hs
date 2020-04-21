import           Bloc
import           Data.List
import           Estat
import           Posicio
import           System.IO
import           Tauler

main :: IO ()
main = do
  --nomfitxer <- getLine
  fitxer <- readFile ("../test/" ++ "facil.txt")
  let ll = lines fitxer

  --putStrLn ("Fitxer " ++ nomfitxer ++ " carregat: ")
  creaMon ll

creaMon :: [String] -> IO()
creaMon l = do
  let estat = sortida l
  print estat
  print (resolt estat)
