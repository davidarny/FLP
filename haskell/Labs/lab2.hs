import System.IO
import Data.List hiding (insert)
import Data.Maybe

do_my_list :: Int-> [Int]
do_my_list n = [n..n * 2 - 1]

oddEven :: [a] -> [a]
oddEven [] = []
oddEven [x] = [x]
oddEven (x1:x2:xs) = x2:x1:(oddEven xs)

-- @see https://stackoverflow.com/a/19170606
insert :: [a] -> a -> Int -> [a]
insert xs x n = as ++ (x:bs)
                where (as,bs) = splitAt n xs

listSum :: Num a => [a] -> [a] -> [a]
listSum a b = zipWith (+) a b

position :: Eq a => a -> [a] -> Int
position atom list = fromMaybe (-1) (findIndex (==atom) list)

main :: IO ()
main = do
       putStr "\"N1_output\"\n"
       list1 <- readLn
       print (oddEven (do_my_list list1))

       putStr "N2:"
       list2 <- readLn
       pos <- readLn
       atom1 <- readLn
       print (insert (do_my_list list2) atom1 pos)

       putStr "N3:"
       list31 <- readLn
       list32 <- readLn
       print (listSum (do_my_list list31) (do_my_list list32))

       putStr "N4:"
       list4 <- readLn
       atom2 <- readLn
       print (position atom2 (do_my_list list4))

       putStr "N5:"
       n1 <- readLn
       print (foldl1 (+) [1..n1])

       putStr "N6:"
       n2 <- readLn
       print (foldl1 (+) (map (\x -> n2 -x) [1..n2]))
