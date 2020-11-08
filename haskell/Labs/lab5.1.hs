list :: Int -> Int -> Int -> [Int]
list x y z = take y (filter (\v -> v `mod` z == 0) ([x..]))

main :: IO()
main = do
  putStrLn "Starting from:"
  x <- readLn
  putStrLn "taking:"
  y <- readLn
  putStrLn "divided by:"
  z <- readLn
  print (list (x :: Int) (y :: Int) (z :: Int))
