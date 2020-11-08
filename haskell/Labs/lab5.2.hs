import System.IO

copy :: String -> String -> Char -> IO()
copy from to char = do
    contents <- readFile from
    writeFile to (map (\ch -> if ch `elem` ['.', ',', '!', '?'] then char else ch) contents)

main :: IO()
main = do
    putStrLn "Copy from:"
    from <- getLine
    putStrLn "Copy to:"
    to <- getLine
    putStrLn "Changing punctuation chars to:"
    char <- getLine
    copy from to (char !! 0)
