import Data.List
import System.IO

-- To execute GHCi
-- To Load Program   :l
-- To Reload program :r
-- To View Declaration of a Function :t

sumOfNums = sum [1..1000]
modEx = mod 6 4
negNumEx = 5 + (-4)

-- fromIntegral converteix Integral a floatingPoint
num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)

-- Built in math functions
piVal           = pi
ePow9         = exp 9
logOf9        = log 9
squared9      = 9 ** 2
truncateVal  = truncate 9.999
roundVal      = round 9.999
ceilingVal    = ceiling 9.999
floorVal       = floor 9.999
-- Also sin, cos, tan, asin, atan, acos, sinh, tanh, cosh, atanh, acosh

-- Operadors
trueAndFalse = True && False
trueOrFalse    = True || False
notTrue           = not( True )

-- Lists
numbers = [3,5,6]
moreNumbers = numbers ++ [7,8,9]
convineNumb = 2 : 7 : 21 : 66 : []  --Crea una llista amb els numeros 2, 7, 21, 66
multList = [[3,5,7], [1,2,3]]
mesNum = 1 : 7 : numbers

lenNu = length numbers  -- Nombre de valors a la llista
revertNu = reverse numbers  --Llista al revés
isEmpty = null numbers  -- Diu si està vuit
secondPrime = numbers !! 1 -- agafa el índex 1 de la llista
firstPrime = head numbers -- Agafa el Primer
lastPrime = last numbers -- Agafa l'ultim
primeInit = init numbers --Treu l'ultim de la llista
first3Primer = take 3 numbers -- Agafa els 3 primers
removedPrimer = drop 3 numbers --Elimina els 3 primers
is7inList = 7 `elem` numbers --Busca el 7 a la Llista
maxPrime = maximum numbers --Màxim
minPrimer = minimum numbers --Mínim
produ = product numbers --Producte de tots els elements

-- Creació de llistes --
zeroToTen = [0..10]
evenList = [2,4..20] --Llista de parells
lleterList = ['A', 'C'..'Z'] --Llista de lletres 'imparells'
