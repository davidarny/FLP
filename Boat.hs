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
startPosition = listArray (Wolf, Farmer) [L, L, L, L]
goalPosition = listArray (Wolf, Farmer) [R, R, R, R]

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
search :: [[Solution]]
search = iterate stepSolution [[startPosition]]

-- нахождение первого решения, которое является ответом
solution :: [Position]
solution = head $ filter ((==goalPosition).head) $ concat $ search

-- вывод решения на экран
showSolution = sequence $ map (putStrLn.show.assocs) solution
