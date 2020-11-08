-- 1. Определите функцию listnums. Она берет численный аргумент n  и возвращает список всех чисел от n до 1, включительно. #2
listnums :: Int -> [Int]
listnums n
    | n <= 1 = [1]
    | otherwise = n:listnums(n - 1)

-- 2. Определите функцию secondlastlist. Эта функция берет список  списков и возвращает последние элементы каждого, объединенные  в список. #2
secondlastlist :: [[a]] -> [a]
secondlastlist (x:xs)
    | length xs == 0 = [last x]
    | otherwise = (secondlastlist [x]) ++ (secondlastlist xs)

-- 3. Определите функцию myunion, которая находит объединение двух  списков. Объединением двух списков будет список содержащий элементы,  которые есть по крайней мере в одном из списков. #2
myunion :: Ord a => [a] -> [a] -> [a]
myunion a [] = a
myunion [] b = b
myunion (x:xs) (y:ys) = case compare x y of
   LT -> x : myunion xs (y:ys)
   EQ -> x : myunion xs ys
   GT -> y : myunion (x:xs) ys

-- 4. Определите функцию mysubst. Получив два списка, она возвращает их разность. Разность двух списков называется список, состоящий из элементов  первого списка, которые не принадлежат второму списку. #2
mysubst :: Ord a => [a] -> [a] -> [a]
mysubst [] _ = []
mysubst a [] = a
mysubst (x:xs) (y:ys) = case compare x y of
   LT -> x : mysubst xs (y:ys)
   EQ -> mysubst xs ys
   GT -> y : mysubst (x:xs) ys

-- 5. Напишите функцию, берущую список списков и возвращающую список из N -х элементов подсписков с помощью функций map и (!!) #2
itemindex :: [[a]] -> Int -> [a]
itemindex xs n = map (\ xss -> xss !! n) xs


main :: IO()
main = do
    putStr "N1:"
    print (listnums 5)
    -- => [5,4,3,2,1]

    putStr "N2:"
    print (secondlastlist [[1, 2, 3], [4, 5, 6]])
    -- => [3,6]

    putStr "N3:"
    print (myunion [1, 2, 3, 4] [2, 3, 4, 5])
    -- => [1,2,3,4,5]

    putStr "N4:"
    print (mysubst [1, 2, 3, 4, 5, 6] [2, 3, 4, 5])
    -- => [1,6]

    putStr "N5:"
    print (itemindex [[1, 2], [3, 4]] 0)
    --  => [1,3]
