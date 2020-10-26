import Data.Char
import Data.List hiding (transpose, findIndices, elemIndices)

-- 1. Напишите функцию elemIndices :: Eq a => a -> [a] -> [Int], которая находит, под какими индексами в списке встречается заданный элемент
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x = findIndices (x==)

-- 2. Напишите функцию unevenHandWritingMy :: String-> String, которая берет строку и возвращает ее же, но каждая третья буква  должна стать прописной, если была строчной и наоборот.
unevenHandWritingMy :: String -> String
unevenHandWritingMy str = map (\ c -> if (fst c) `mod` 3 == 0 then if isLower (snd c) then toUpper (snd c) else toLower (snd c) else snd c) charByIndex
    where charByIndex = zip [1..(length str)] str

-- 3. Напишите функцию, строящую список подсписков чисел: в первом подсписке будут степени единицы, во втором степени двойки, в третьем - тройки и так далее.

listOfLists :: Int -> Int -> [[Int]]
listOfLists i n = map (\ x -> take i (iterate(*x) x) ) [1, 2..n]

-- 4. Напишите функцию transpose :: [[a]]-> [[a]], которая берет список списков и транспонирует столбцы и строки.
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- 5. Напишите функцию, которая читает входной текстовый файл и выводит в выходной файл пары (слово:число), где слово - есть каждое уникальное слово файла, а число - количество вхождений этого слова. Пары должны быть отсортированы по убыванию чисел
uniqWordsStats :: String -> String -> IO ()
uniqWordsStats inFileName outFileName = do
    contents <- readFile inFileName
    let contentWords = words contents
    let uniqContentWords = nub contentWords
    let wordsStats = map (\ word -> (length (filter (word==) contentWords), word) ) uniqContentWords
    writeFile outFileName (intercalate "\n" (map (\ stat -> (show (fst stat)) ++ " " ++ (snd stat)) wordsStats))
