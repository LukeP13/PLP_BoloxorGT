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
    show (Trobada ((e,m):list)) = "Inici\n" ++ show e ++ mostraMovim list 1
      where mostraMovim [] _ = []
            mostraMovim ((e,m):xl) i = show i ++ " " ++ show m ++ "\n" ++ show e ++ mostraMovim xl (i+1)

  buscaSolucio :: Estat -> Solucio
  buscaSolucio = pasSolucio (Trobada [])

  pasSolucio :: Solucio -> Estat -> Solucio
  pasSolucio Inexistent _ = Inexistent
  pasSolucio sol estat
    | resolt estat = sol


  dijkstra :: Estat -> Solucio
  dijkstra estat = buscaMillor $ iDijkstra vist dist
    where vist = modificaDist [] [estat] False   -- Afegim l'estat inicial
          dist = modificaDist [] [estat] (Trobada [(estat, Invalid)]) -- Afegim distància al estat inicial


  iDijkstra :: [(Estat, Bool)] -> [(Estat, Solucio)] -> [(Estat, Solucio)]
  iDijkstra visit dist
    | totsVisitats visit = dist
    | otherwise = iDijkstra visita newDist
      where e = agafaNoVisitat visit -- OK Agafem el primer estat no visitat
            movLegals (Estat b t) = legals t b -- OK
            leg = movLegals e    -- OK Agafem tots els moviments legals
            estatsDest = getNousEstats e leg -- OK
            nousEstats = [x | x <- estatsDest, x `notElem` getkeys visit] -- OK
            newVisit = modificaDist visit nousEstats False  -- Afegim els estats al que podem arribar amb 'false'
            visita = modificaDist newVisit [e] True

            newDist = afegeixMenors dist (listOfTuples estatsDest leg) (getVal dist e)


  totsVisitats :: [(Estat, Bool)] -> Bool
  totsVisitats [] = True
  totsVisitats ((e, b):l) = b && totsVisitats l

  agafaNoVisitat :: [(Estat, Bool)] -> Estat
  agafaNoVisitat [] = error "Tots els nodes visitats"
  agafaNoVisitat ((e,b):l)
    | b = agafaNoVisitat l
    | otherwise = e

  afegeixMenors :: [(Estat, Solucio)] -> [(Estat, Moviment)] -> Solucio -> [(Estat, Solucio)]
  afegeixMenors dist [] _ = dist
  afegeixMenors dist ((eB,m):x) sol
    | eB `notElem` getkeys dist = afegeixMenors (afegeixPas dist (eB,m) sol) x sol
    | length' (getVal dist eB) > length' sol = afegeixMenors (afegeixPas dist (eB,m) sol) x sol
    | otherwise = afegeixMenors dist x sol


  getNousEstats :: Estat -> [Moviment] -> [Estat]
  getNousEstats _ [] = []
  getNousEstats eIni (m:ml) = execMovim eIni m : getNousEstats eIni ml


  modificaDist :: Eq a => [(a, b)] -> [a] -> b -> [(a, b)]
  modificaDist dist [] _ = dist
  modificaDist dist (e:el) pes
    | posFound >= 0 = modificaDist (updateList dist (e,pes) posFound) el pes
    | otherwise = modificaDist (dist ++ [(e,pes)]) el pes
    where posFound = posElem dist e


  afegeixPas ::[(Estat, Solucio)] -> (Estat, Moviment) -> Solucio -> [(Estat, Solucio)]
  afegeixPas dist (e,m) Inexistent = error "puta"
  afegeixPas dist (e,m) (Trobada sol) = modificaDist dist [e] (Trobada (sol++[(e,m)]))

  buscaMillor :: [(Estat, Solucio)] -> Solucio
  buscaMillor [] = Inexistent
  buscaMillor ((e,s):l)
    | trobada s && length' s < length' solAnt = s
    | otherwise = solAnt
    where solAnt = buscaMillor l

  trobada :: Solucio -> Bool
  trobada Inexistent = False
  trobada (Trobada l) = resolt $ fst $ last l

  length' :: Solucio -> Int
  length' Inexistent = 999
  length' (Trobada l) = length l

  getkeys :: [(a,b)] -> [a]
  getkeys list = [x | (x, _) <- list]

  getVals :: [(a,b)] -> [b]
  getVals list = [x | (_, x) <- list]

  getVal :: Eq a => [(a,Solucio)] -> a -> Solucio
  getVal [] _ = Inexistent
  getVal ((x,s):l) y | x == y = s
                     | otherwise = getVal l y
