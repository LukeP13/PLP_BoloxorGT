import           Bloc
import           Data.Char
import           Data.List
import           Estat
import           Moviment
import           Posicio
import           Resolver
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
  dijkstra estat
  -- let solucio = buscaSolucio estat
  -- print solucio

juga :: Estat -> Int -> IO ()
juga _ (-1) = putStrLn "*************** FI *****************"
juga estat i
  | resolt estat = putStrLn $ show estat ++ "\n**************** RESOLT *****************"
  | otherwise = do
    putStrLn $ "*************** Jugada " ++ show i ++ " ****************"
    print estat
    putStrLn "Cap on vols moure [w/a/s/d] [e per sortir]?"
    moviment <- getChar
    putStrLn  "\n*****************************************\n"

    if moviment == 'e'
      then juga estat (-1)
      else mov estat (creaMoviment moviment) i


mov :: Estat -> Moviment -> Int -> IO ()
mov e Invalid i = do
  putStrLn " MOVIMENT INVÀLID | Torna a seleccionar moviment "
  juga e i
mov e m i
  | fora e m = juga e (-1)
  | otherwise = do
    let newE = execMovim e m
    juga newE (i+1)
