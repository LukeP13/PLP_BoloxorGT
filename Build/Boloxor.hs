import           Bloc
import           Data.List
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

creaMon :: [[Char]] -> IO()
creaMon (x:y:z:list) = do
  let bloc = creaBloc (read x)
  let tauler = creaTauler (read y) (read z) list
  putStrLn(show tauler)
  --show tauler
