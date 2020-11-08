main :: IO()

-- @see https://stackoverflow.com/a/48900096
odds :: Int -> [Int]
odds = flip take [1,3..]

triangular :: Int -> Int
triangular x = x * (x + 1) `div` 2

-- @see https://stackoverflow.com/a/19063281
tri_series :: Int -> [Int]
tri_series x = map triangular [1..x]

-- @see https://en.wikipedia.org/wiki/square_pyramidal_number
pyramid :: Int -> Int
pyramid x = (x * (x + 1) * (2 * x + 1)) `div` 6

pyr_series :: Int -> [Int]
pyr_series x = map pyramid [1..x]

main = do
    putStr "N1:"
    print (snd(fst((1, 'a'), "abc")))
    putStr "N2:"
    print (head(tail ['a', 'b', 'c']))
    putStr "N2:"
    print (tail(head [['a', 'b'], ['c','d']]) !! 0)
    putStr "N2:"
    print (tail(head(tail([['a', 'c', 'd'], ['a','b']]))) !! 0)
    putStr "N2:"
    print (head(head(tail([['a','d'], ['b', 'c']]))))
    putStr "N3:"
    print ([ x | x <- [1..40], x `mod` 2 == 1])
    putStr "N3:"
    print ([ x | x <- [1..40], odd x])
    putStr "N3:"
    print (odds 20)
    putStr "N4:"
    print (tri_series 50)
    putStr "N5:"
    print (pyr_series 50)
