import System.Environment
import System.IO
import Data.List
import Data.Maybe
import Data.Char

import Core.Todo

import Core.Virtualizer
    (
        mkWorld,
    )

import Core.Base
    (
        World(..),
        Command(..),
    )

data Mode = AI | Human | Quit deriving (Show, Eq)
newtype ModeError = ModeError String deriving (Show, Eq)

fromCharToMaybeMode :: Char -> Maybe Mode
fromCharToMaybeMode c =
    case toUpper c of
        'A' -> return AI
        'H' -> return Human
        'Q' -> return Quit
        _ -> Nothing

fromCharToMaybeCommand :: Char -> Maybe Command
fromCharToMaybeCommand c =
    case toUpper c of
        'A' -> return Left_
        'W' -> return Up_
        'D' -> return Right_
        'S' -> return Down_
        _ -> Nothing

render :: World -> IO()
render = putStrLn . show

selectCommandMessage :: String
selectCommandMessage = "(a) left" ++ ", (d) right" ++ ", (w) up" ++ ", (s) down"

play :: World -> IO ()
play w = do
            putStrLn selectCommandMessage
            render w
            char <- getChar
            breakLine
            case fromCharToMaybeCommand char of
              Nothing -> putStrLn selectCommandMessage
              Just c -> putStrLn "Great!"

solve :: World -> IO ()
solve _ = todo

selectMode :: String -> IO (Either ModeError Mode)
selectMode txt =
    do
        putStrLn txt
        maybe (Left $ ModeError "Invalid mode") Right . fromCharToMaybeMode . toLower
            <$> getChar

selectModeMessage :: String
selectModeMessage = "Select mode (q)uit or (a)I or (h)uman: "

breakLine :: IO ()
breakLine = putStrLn ""

run :: World -> IO ()
run world =
    do
        mode <- selectMode selectModeMessage
        breakLine
        case mode of
            Left (ModeError err) -> putStrLn err >> run world
            Right Quit -> putStrLn "Bye!"
            Right AI -> putStrLn "AI mode" >> solve world
            Right Human -> putStrLn "Human mode" >> play world

main :: IO ()
main =
    do
        content <- readFile "Samples/world.txt"
        case mkWorld $ lines content of
            Left err -> error $ "Error - " ++ show err
            Right val -> run val

