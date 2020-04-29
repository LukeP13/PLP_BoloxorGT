module Resolver where
  import System.IO
  import Posicio
  import Bloc
  import Estat
  import Moviment
  import Tauler

  -- Mètodes Genèrics

  -- Retorna la posició on es troba la tupla amb la clau entrada (si existeix)
  posElem :: Eq a => [(a, b)] -> a -> Int
  posElem = buscaElem 0
    where buscaElem _ [] _ = -1
          buscaElem i ((x,_):xl) y
              | x == y = i
              | otherwise = buscaElem (i+1) xl y


  -- Tipus Solució
  data Solucio = Trobada [(Estat, Moviment)] | Inexistent
                deriving Eq
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

  -- Executa l'algorisme de Dijkstra per trobar la millor solució a partir d'un estat
  dijkstra :: Estat -> Solucio
  dijkstra estat = buscaMillor $ iDijkstra vist dist
    where vist = modValor [] [estat] False   -- Afegim l'estat inicial
          dist = modValor [] [estat] (Trobada [(estat, Invalid)]) -- Afegim distància al estat inicial


  -- Funció recursiva d'inmersió que executa l'algorisme de Dijkstra i retorna una solució si hi ha
  iDijkstra :: [(Estat, Bool)] -> [(Estat, Solucio)] -> [(Estat, Solucio)]
  iDijkstra visit dist
    | totsVisitats visit = dist
    | otherwise = iDijkstra visita newDist
      where e = agafaNoVisitat visit -- OK Agafem el primer estat no visitat
            movLegals (Estat b t) = legals t b -- OK
            leg = movLegals e    -- OK Agafem tots els moviments legals
            estatsDest = getNousEstats e leg -- OK
            nousEstats = [x | x <- estatsDest, x `notElem` getkeys visit] -- OK
            newVisit = modValor visit nousEstats False  -- Afegim els estats al que podem arribar amb 'false'
            visita = modValor newVisit [e] True

            newDist = afegeixMenors dist (listOfTuples estatsDest leg) (getVal dist e)

  -- Retorna cert si tots els estats de la llista han estat visitats o fals altrament
  totsVisitats :: [(Estat, Bool)] -> Bool
  totsVisitats [] = True
  totsVisitats ((e, b):l) = b && totsVisitats l

  -- Retorna el primer estat que trobem com a no visitat a la llista (si hi ha algun no visitat)
  agafaNoVisitat :: [(Estat, Bool)] -> Estat
  agafaNoVisitat [] = error "Tots els nodes visitats" -- Error en cas d'haver visitat ja tots els nodes
  agafaNoVisitat ((e,b):l)
    | b = agafaNoVisitat l
    | otherwise = e

  afegeixMenors :: [(Estat, Solucio)] -> [(Estat, Moviment)] -> Solucio -> [(Estat, Solucio)]
  afegeixMenors dist [] _ = dist
  afegeixMenors dist ((eB,m):x) sol
    | eB `notElem` getkeys dist = afegeixMenors (afegeixPas dist (eB,m) sol) x sol
    | length' sol `betw` (0 ,length' solAnt)  = afegeixMenors (afegeixPas dist (eB,m) sol) x sol
    | otherwise = afegeixMenors dist x sol
    where solAnt = getVal dist eB

  getNousEstats :: Estat -> [Moviment] -> [Estat]
  getNousEstats _ [] = []
  getNousEstats eIni (m:ml) = execMovim eIni m : getNousEstats eIni ml

  modValor :: Eq a => [(a, b)] -> [a] -> b -> [(a, b)]
  modValor dist [] _ = dist
  modValor dist (e:el) pes
    | posFound >= 0 = modValor (updateList dist (e,pes) posFound) el pes
    | otherwise = modValor (dist ++ [(e,pes)]) el pes
    where posFound = posElem dist e

  afegeixPas ::[(Estat, Solucio)] -> (Estat, Moviment) -> Solucio -> [(Estat, Solucio)]
  afegeixPas dist _ Inexistent = dist
  afegeixPas dist (e,m) (Trobada sol) = modValor dist [e] (Trobada (sol++[(e,m)]))

  -- Busca la millor solució de la llista
  buscaMillor :: [(Estat, Solucio)] -> Solucio
  buscaMillor [] = Inexistent
  buscaMillor ((e,s):l)
    | trobada s && (not (trobada solAnt) || length' s < length' solAnt) = s
    | otherwise = solAnt
    where solAnt = buscaMillor l

  -- Retona cert en cas que el darrer moviment de la solució sigui resolt o fals altrament
  trobada :: Solucio -> Bool
  trobada Inexistent = False
  trobada (Trobada l) = resolt $ fst $ last l -- De l'ultim moviment de la solució agafa l'estat i comprova si està resolt

  -- Retorna el nombre de passes de la solució
  length' :: Solucio -> Int
  length' Inexistent = -1
  length' (Trobada l) = length l

  -- Retorna les claus de la llista de parelles
  getkeys :: [(a,b)] -> [a]
  getkeys list = [x | (x, _) <- list]

  -- Retorna els valors de la llista de parelles
  getVals :: [(a,b)] -> [b]
  getVals list = [x | (_, x) <- list]

  -- Retorna la solució que la seva clau coincideix amb el valor entrat
  getVal :: Eq a => [(a,Solucio)] -> a -> Solucio
  getVal [] _ = Inexistent
  getVal ((x,s):l) y | x == y = s
                     | otherwise = getVal l y
