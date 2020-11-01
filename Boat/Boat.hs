module Boat where
import Data.List
import Data.Array

-- лодка и волк-коза-капуста
-- объект
data Item = Wolf | Goat | Cabbage | Farmer
    deriving (Show, Eq, Ord, Ix)

-- положение
data Location = L | R
    deriving (Show, Eq, Ord)

-- обратное положение
from L = R
from R = L

-- позиция: где кто
type Position = Array Item Location

-- начальная и целевая позиция
startPositionA = listArray (Wolf, Farmer) [L, L, L, L]
goalPositionA = listArray (Wolf, Farmer) [R, R, R, R]

startPositionB = listArray (Wolf, Farmer) [L, L, L, L]
goalPositionB = listArray (Wolf, Farmer) [R, R, R, L]

startPositionC = listArray (Wolf, Farmer) [L, L, R, L]
goalPositionC = listArray (Wolf, Farmer) [R, R, R, R]

startPositionD = listArray (Wolf, Farmer) [L, L, L, L]
goalPositionD = listArray (Wolf, Farmer) [R, R, L, R]

startPositionE = listArray (Wolf, Farmer) [L, R, L, L]
goalPositionE = listArray (Wolf, Farmer) [R, R, R, R]

-- неправильная позиция: без контроля человека остаются
-- волк с козлом или козел с капустой
wrongPosition :: Position -> Bool
wrongPosition p =
    all (/= p!Farmer) [p!Wolf, p!Goat] || all (/= p!Farmer) [p!Cabbage, p!Goat]

-- шаг переправы с берега на берег с кем-нибудь: какие варианты можно получить
step :: Position -> [Position]
step p =
    [p // [(who, from wher)] // [(Farmer, from wher)] | (who,wher) <- whom] where
    whom = filter ((== p!Farmer) . snd) $ assocs p

-- решение: последовательность позиций (самая последняя - в начале списка)
type Solution = [Position]

-- построение нового списка возможных решений из старого
stepSolution :: [Solution] -> [Solution]
stepSolution sols =
    [(newpos:sol) | sol <- sols, newpos <- step (head sol), not $ wrongPosition newpos]

-- итеративный процесс построения возможных решений,
-- для поиска среди них того, которое является ответом
searchA :: [[Solution]]
searchA = iterate stepSolution [[startPositionA]]

searchB :: [[Solution]]
searchB = iterate stepSolution [[startPositionB]]

searchC :: [[Solution]]
searchC = iterate stepSolution [[startPositionC]]

searchD :: [[Solution]]
searchD = iterate stepSolution [[startPositionD]]

searchE :: [[Solution]]
searchE = iterate stepSolution [[startPositionE]]

-- нахождение первого решения, которое является ответом
solutionA :: [Position]
solutionA = head $ filter ((==goalPositionA).head) $ concat $ searchA

solutionB :: [Position]
solutionB = head $ filter ((==goalPositionB).head) $ concat $ searchB

solutionC :: [Position]
solutionC = head $ filter ((==goalPositionC).head) $ concat $ searchC

solutionD :: [Position]
solutionD = head $ filter ((==goalPositionD).head) $ concat $ searchD

solutionE :: [Position]
solutionE = head $ filter ((==goalPositionE).head) $ concat $ searchE

-- вывод решения на экран
showSolutionA = sequence $ map (putStrLn.show.assocs) solutionA

showSolutionB = sequence $ map (putStrLn.show.assocs) solutionB

showSolutionC = sequence $ map (putStrLn.show.assocs) solutionC

showSolutionD = sequence $ map (putStrLn.show.assocs) solutionD

showSolutionE = sequence $ map (putStrLn.show.assocs) solutionE
