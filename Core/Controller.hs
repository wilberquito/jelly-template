module Controller
    (
        Movement(..),
        play,
        moveJelly,
        suckedDown,
        reachedGoal,
        printWorld
    ) where

import System.IO
import Data.List
import Control.Monad
import Data.Maybe

import Definitions
    (
        World(..),
        Jelly(..),
        Area(..),
        Movement(..),
        Table,
        Volume,
        Point,
        toChar,
        nearAndFarPoints
    )
    

toMovement :: Char -> Maybe Movement
toMovement c 
    | c == 'w' || c == 'W' || c == 'i' || c == 'I' = Just Upward
    | c == 's' || c == 'S' || c == 'k' || c == 'K' = Just Downward
    | c == 'a' || c == 'A' || c == 'j' || c == 'J' = Just Leftward
    | c == 'd' || c == 'D' || c == 'l' || c == 'L' = Just Rightward
    | otherwise = Nothing

moveJelly :: Jelly -> Movement -> Jelly
moveJelly (Jelly((a,b),(x,y,z))) movement
    | movement == Upward = Jelly((a,b-z),(x,z,y))  
    | movement == Downward = Jelly((a,b+y),(x,z,y))
    | movement == Leftward = Jelly((a-z,b),(z,y,x))
    | otherwise = Jelly((a+x,b),(z,y,x))

whereIsJelly :: Jelly -> [Point]
whereIsJelly (Jelly((a,b),(x,y,_))) = [ (ai,bi) | ai <- [a..a'], bi <- [b..b'] ]
    where
        (a',b') = (a+x-1,b+y-1)

-- this function checks if Jelly is on the meta and if it fixes in      
reachedGoal :: Jelly -> Table -> Bool
reachedGoal jelly tbl = jellyFits points tbl
    where
        points = whereIsJelly jelly


jellyFits :: [Point] -> Table -> Bool
jellyFits pointsJelly tbl = diffPoints == []
    where
        ((x0,y0),(x1,y1)) = nearAndFarPoints tbl Goal
        pointsGoal = [ (xi,yi) | xi <- [x0..x1], yi <- [y0..y1] ]
        diffPoints = [ pi | pi <- pointsJelly, not $ pi `elem` pointsGoal ]


suckedDown :: Jelly -> Table -> Bool
suckedDown jelly@(Jelly((a,b),(x,y,_))) tbl = outOfTable || suckedDownWithInTable jelly tbl
    where
        outOfTable = a < 0 || b < 0 || (a+x) > length (head tbl) || (b+y) > length tbl


suckedDownWithInTable :: Jelly -> Table -> Bool
suckedDownWithInTable jelly@(Jelly((a,b),volume)) tbl = brokenIce points tbl volume || holeIgnored points tbl 
    where
        points = whereIsJelly jelly


-- if there is a ice in the table in any point of the collection,
-- the ice breaks if Jelly is lying with its major pressure possible
brokenIce :: [Point] -> Table -> Volume -> Bool
brokenIce points tbl (x,y,z) = thereIsIce points tbl && x*y <= y*z && x*y <= x*z 


thereIsIce :: [Point] -> Table -> Bool
thereIsIce points table = appearsThisArea points table Ice 


holeIgnored :: [Point] -> Table -> Bool
holeIgnored points table = appearsThisArea points table Hole


appearsThisArea :: [Point] -> Table -> Area -> Bool
appearsThisArea [] _ _ = False
appearsThisArea ((a,b):xs) tbl area = tbl !! b !! a == area || appearsThisArea xs tbl area


-- given the layout of the output I put on the layout Jelly's representation
generateOutput :: [String] -> Char -> [(Int,Int)] -> String
generateOutput canva _ [] = unlines canva
generateOutput canva jelly ((a,b):xs) = generateOutput canva' jelly xs
    where
        head = take a $ canva !! b
        tail = drop (a+1) $ canva !! b
        line = head ++ [jelly] ++ tail
        canva' = take b canva ++ [line] ++ drop (b+1) canva 


printWorld :: World -> IO()
printWorld (World(jelly,table)) = putStr $ generateOutput canva 'B' points 
    where
        canva = [ map toChar array | array <- table ]
        points = whereIsJelly jelly


play :: World -> IO()
play world@(World(jelly,table)) = do
                printWorld world
                line <- getLine
                if line /= "*"
                then do
                    let movement = toMovement $ head line
                    if isJust movement
                    then do
                        let jelly' = moveJelly jelly (fromJust movement)
                        if reachedGoal jelly' table
                        then
                            putStrLn "Has pasado el nivel, ¡Felicidades!"
                        else do
                            if suckedDown jelly' table -- aspirado
                            then
                                putStrLn "Se acabó, has caido al vacio"
                            else
                                play $ World(jelly', table)
                    else do
                        putStrLn "Movimiento impossible!"
                        play world
                else
                    putStrLn "UNA LASTIMA QUE NO QUIERAS SEGUIR JUGANDO... D':"
