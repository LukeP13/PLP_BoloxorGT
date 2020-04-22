import           Bloc
import           Data.Char
import           Data.List
import           Estat
import           Posicio
import           System.IO
import           Tauler

main :: IO ()
main = do
  putStrLn "Entra el nom del fitxer de configuració -> "
  nomfitxer <- getLine
  fitxer <- readFile ("../test/" ++ nomfitxer)
  let ll = lines fitxer

  -- Carrega i Mostra estat inicial
  let estatIni = sortida ll
  putStrLn $ "Fitxer " ++ nomfitxer ++ " carregat correctament\nTauler inicial: "
  putStrLn $ mostraMon estatIni

  -- Determinar si resoldre jugador o automàtic
  putStrLn "Vols jugar o resoldre automàticament [j/r] -> "
  resposta <- getLine

  if resposta == "j"
    then juga estatIni 1
    else resol estatIni

resol :: Estat -> IO ()
resol estat = do
  print estat

juga :: Estat -> Int -> IO ()
juga _ 0 = putStrLn "*************** FI ******************"
juga estat i = do
  putStrLn $ "*************** Jugada " ++ show i ++ " ****************"
  print estat
  putStrLn "Cap on vols moure [w/a/s/d] [exit per sortir]?"
  moviment <- getLine
  putStrLn  "*****************************************\n"

  if moviment == "exit"
    then juga estat 0
    else juga estat (i+1)





mou :: Estat -> IO ()
mou estat = putStr ""
