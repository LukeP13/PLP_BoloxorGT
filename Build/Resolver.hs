module Resolver where
  import System.IO
  import Posicio
  import Bloc
  import Estat
  import Moviment
  import Tauler

  -- Genèrics
  posElem :: Eq a => [(a, b)] -> a -> Int
  posElem = buscaElem 0
    where buscaElem _ [] _ = -1
          buscaElem i ((x,_):xl) y
              | x == y = i
              | otherwise = buscaElem (i+1) xl y


  -- Tipus Solució
  data Solucio = Trobada [(Estat, Moviment)] | Inexistent
  instance Show Solucio where
    show Inexistent = "No s'ha trobat solució"
    show (Trobada []) = "Solucio buida"
    show (Trobada ((e,m):list)) = "Inici\n" ++ show e ++ mostraMovim list
      where mostraMovim [] = []
            mostraMovim ((e,m):xl) = show m ++ "\n" ++ show e ++ mostraMovim xl

  buscaSolucio :: Estat -> Solucio
  buscaSolucio = pasSolucio (Trobada [])

  pasSolucio :: Solucio -> Estat -> Solucio
  pasSolucio Inexistent _ = Inexistent
  pasSolucio sol estat
    | resolt estat = sol


  dijkstra :: Estat -> IO()
  dijkstra (Estat bloc taul) = do
    let solucio = Trobada [(estat, Invalid)]
    let vist = modificaDist [] [estat] True    -- Marquem visitat l'estat incial
    let dist_aux = modificaDist [] [estat] solucio -- Afegim distància al estat inicial

    let leg = legals taul bloc
    let dist = afegeixPas dist_aux (listOfTuples (getNousEstats estat leg) leg) solucio -- Afegim un pas a les solucions dels estats seguents


    print dist


    where estat = Estat bloc taul


  getNousEstats :: Estat -> [Moviment] -> [Estat]
  getNousEstats _ [] = []
  getNousEstats eIni (m:ml) = execMovim eIni m : getNousEstats eIni ml


  modificaDist :: Eq a => [(a, b)] -> [a] -> b -> [(a, b)]
  modificaDist dist [] _ = dist
  modificaDist dist (e:el) pes
    | posFound >= 0 = modificaDist (updateList dist (e,pes) posFound) el pes
    | otherwise = modificaDist (dist ++ [(e,pes)]) el pes
    where posFound = posElem dist e


  afegeixPas ::[(Estat, Solucio)] -> [(Estat, Moviment)] -> Solucio -> [(Estat, Solucio)]
  afegeixPas dist [] _ = dist
  afegeixPas dist ((e,m):xs) (Trobada sol)  = afegeixPas (modificaDist dist [e] (Trobada (sol ++ [(e,m)]))) xs (Trobada sol)


  getkeys :: [(a,b)] -> [a]
  getkeys list = [x | (x, _) <- list]

  getVal :: [(a,b)] -> [b]
  getVal list = [x | (_, x) <- list]
