module Loop where

import Prelude hiding (Either(..))
import Data.Char (isDigit)
import Game

data Query = Quit | NewGame Int | Play Move

-- Основные функции

play :: IO ()
play = greetings >> setup >>= gameLoop

setup :: IO Game
setup = putStrLn "Let's start new game?" >>
    putStrLn "Insert difficulty (integer number): " >>
    getLine >>= maybe setup shuffle . readInt


gameLoop :: Game -> IO ()
gameLoop game
    | isGameOver game   = showResults game >> setup >>= gameLoop
    | otherwise         = showGame game >> askForMove >>= reactOnMove game

-- Запросы пользователя


reactOnMove :: Game -> Query -> IO ()
reactOnMove game query = case query of
    Quit        -> quit
    NewGame n   -> gameLoop =<< shuffle n
    Play    m   -> gameLoop $ move m game

askForMove :: IO Query
askForMove = showAsk >>
    getLine >>= maybe askAgain return . parseQuery
    where askAgain = wrongMove >> askForMove


parseQuery :: String -> Maybe Query
parseQuery x = case x of
    "up"    -> Just $ Play Up
    "u"     -> Just $ Play Up
    "down"  -> Just $ Play Down
    "d"     -> Just $ Play Down
    "left"  -> Just $ Play Left
    "l"     -> Just $ Play Left
    "right" -> Just $ Play Right
    "r"     -> Just $ Play Right
    "quit"  -> Just $ Quit
    "q"     -> Just $ Quit

    'n':'e':'w':' ':n   -> Just . NewGame =<< readInt n
    'n':' ':n           -> Just . NewGame =<< readInt n
    _       -> Nothing


readInt :: String -> Maybe Int
readInt n
    | all isDigit n = Just $ read n
    | otherwise     = Nothing


-- Ответы пользователю

greetings :: IO ()
greetings = putStrLn "Hi! This is Fifteen game!" >>
    showGame initGame >>
    remindMoves


showResults :: Game -> IO ()
showResults g = showGame g >> putStrLn "Game over."


showGame :: Game -> IO ()
showGame = putStrLn . show


wrongMove :: IO ()
wrongMove = putStrLn "Wrong move." >> remindMoves


showAsk :: IO ()
showAsk = putStrLn "Your move: "

remindMoves :: IO ()
remindMoves = mapM_ putStrLn talk
    where talk = [
            "Possible moves of empty cell:",
            "   left     or l       -- to left",
            "   right    or r       -- to right",
            "   up       or u       -- to up",
            "   down     or d       -- to down",
            "Other actions:",
            "   new int  or n int -- start new game, int - difficulty",
            "   quit     or q      -- exit game"]

quit :: IO ()
quit = putStrLn "Bye." >> return ()
