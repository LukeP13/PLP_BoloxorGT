import           Bloc
import           Data.Char
import           Data.List
import           Estat
import           Moviment
import           Posicio
import           Resolver
import           System.IO
import           Tauler

-- Funció main que executa el joc
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
    then juga estatIni 1 -- Jugar
    else resol estatIni -- Resoldre automàticament

-- Executar l'algorisme de resolució automàtica que troba la solució més curta (Dijkstra) a partir d'un estat
resol :: Estat -> IO ()
resol estat = do
  let solucio = dijkstra estat -- Executar algorisme de Dijkstra
  print solucio -- Mostrar la solució

-- Algorisme per jugar amb el teclat
juga :: Estat -> Int -> IO ()
juga _ (-1) = putStrLn "*************** FI *****************"
juga estat i
  | resolt estat = putStrLn $ show estat ++ "\n**************** RESOLT *****************" -- Si s'ha resolt acaba
  | otherwise = do -- Si no s'ha resolt anem entrant moviments per teclat i anem mostrant el tauler o sortim  ('w', 'a', 's', 'd', 'e')
    putStrLn $ "*************** Jugada " ++ show i ++ " ****************"
    print estat
    putStrLn "Cap on vols moure [w/a/s/d] [e per sortir]?"
    moviment <- getChar
    putStrLn  "\n*****************************************\n"

    if moviment == 'e'
      then juga estat (-1) -- Sortir
      else modificaEstat estat (creaMoviment moviment) i -- Moure bloc

-- Fer un moviment en un determinat estat
modificaEstat :: Estat -> Moviment -> Int -> IO ()
modificaEstat e Invalid i = do -- Moviment invàlid
  putStrLn " MOVIMENT INVÀLID | Torna a seleccionar moviment "
  juga e i
modificaEstat e m i -- Moviment vàlid
  | fora e m = juga e (-1) -- La peça cau i acaba el joc
  | otherwise = do
    let newE = execMovim e m -- Actualitzar estat amb el moviment efectuat
    juga newE (i+1) -- Juguem amb el nou estat i actualitzem el nombre de jugada i
