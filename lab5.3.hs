import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Char

dispatchCommand :: [(String, [String] -> IO ())]
dispatchCommand =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            , ("copy", copy)
            ]

dispatchFilter :: [(String, Char -> Bool)]
dispatchFilter = [ ("ascii", isAscii)
            , ("number", isNumber)
            ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatchCommand
    action args

add :: [String] -> IO ()
add [fileName, textItem] = appendFile fileName (textItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let textLines = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] textLines
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        textLines = lines contents
        nextTextLines = delete (textLines !! number) textLines
    hPutStr tempHandle $ unlines nextTextLines
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

copy :: [String] -> IO ()
copy [fromFileName, toFileName, filterName] = do
    let (Just filterAction) = lookup filterName dispatchFilter
    inputHandle <- openFile fromFileName ReadMode
    hSetEncoding inputHandle utf8
    hGetContents inputHandle >>= \ s -> writeFile toFileName (filter filterAction s)
