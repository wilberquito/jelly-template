{- 
    @autor: [Cantact me](https://www.instagram.com/wilberquito/)
-}

module Solver
    (
        resolve    
    ) where

import System.IO
import Data.Maybe
import Data.List

import Definitions
    (
        World(..),
        Jelly(..),
        Route(..),
        States(..),
        Movement(..),
        Table(..)
    )

import Controller
    (
        moveJelly,
        suckedDown,
        reachedGoal,
        printWorld
    )

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

resolve :: World -> IO()
resolve world@(World(jelly,table)) = do
                            let solutionRoute = breadthFirst table [] [(jelly,[])]
                            if isJust solutionRoute
                            then do
                                putStrLn "Estado inicial"
                                printWorld world
                                printSolution (fromJust solutionRoute) world
                            else
                                putStrLn "Parece que este mapa no tiene solución..."
                                

printSolution :: Route -> World -> IO()
printSolution [] _ = putStrLn "¡INCREIBLE!"
printSolution (mvnt:xs) (World(jelly,table))= do
                            putStr "Movimiento: "
                            print mvnt
                            let jelly' = moveJelly jelly mvnt
                            printWorld $ World(jelly',table)
                            printSolution xs (World(jelly',table))
                            

-- here we pass the map and a list with the first state of Jelly
-- and returns a Just solution or Nothing. 
-- If the response is Nothing, there is no solution
breadthFirst :: Table -> States -> [(Jelly,Route)] -> Maybe Route
breadthFirst tbl visited lvl =      if isJust solution
                                    then
                                        Just (snd (fromJust solution))
                                    else do
                                        if lvl == []
                                        then 
                                            Nothing
                                        else do
                                            let visited' = updateVisitedStates lvl visited
                                                lvl' = expand tbl visited lvl
                                            breadthFirst tbl visited' lvl'

    where
        -- returns Just (Jelly,Route) if has solution, otherwise return Nothing
        solution = reachedGoal' tbl lvl


reachedGoal' :: Table -> [(Jelly,Route)] -> Maybe (Jelly,Route)
reachedGoal' _ [] = Nothing
reachedGoal' tbl ((jelly,route):xs) =   if reachedGoal jelly tbl
                                        then 
                                            Just (jelly,route)
                                        else
                                            reachedGoal' tbl xs


updateVisitedStates :: [(Jelly,Route)] -> States -> States
updateVisitedStates [] states = states
updateVisitedStates ((jelly,_):xs) states = jelly : updateVisitedStates xs states


-- for each node in the current lvl I generate other nodes if it is possible
-- at the first beggining there is only one node, the root, it can generate at most 4 new nodes
expand :: Table -> States -> [(Jelly,Route)] -> [(Jelly,Route)]
expand _ _ [] = []
expand tbl visited ((jelly,route):xs) = newNodes ++ expand tbl visited xs 
    where
        -- it is needed to rescue only the movements that avoid to return to state already visited
        utilMovements = notCyclicalMovements jelly tbl visited
        newNodes = expandNode (jelly,route) utilMovements


-- given a node that represents the state of jelly in the game, I generate other nodes
-- with the movements that have been passed
expandNode :: (Jelly,Route) -> [Movement] -> [(Jelly,Route)]
expandNode _ [] = []
expandNode (jelly,route) (mvnt:xs) = (jelly', route') : expandNode (jelly,route) xs
    where
        route' = route ++ [mvnt]
        jelly' = moveJelly jelly mvnt


-- avoiding cyclical movements
notCyclicalMovements :: Jelly -> Table -> States -> [Movement]
notCyclicalMovements jelly tbl states = utilMovements
    where
        possibleMovements = forwardMovements (World(jelly,tbl))
        -- ading to the collection only the movements that does not generate states in wich Jelly has been previously
        utilMovements = [ mvnt | mvnt <- possibleMovements, not ((moveJelly jelly mvnt) `elem` states) ]


-- if returs an empty list, then in this state Jelly has no possible movement
forwardMovements :: World -> [Movement]
forwardMovements world = [ mvnt | mvnt <- movements, properMovement mvnt world]
    where 
        movements = [Upward, Downward, Leftward, Rightward]


properMovement :: Movement -> World -> Bool
properMovement mvnt (World(jelly,table)) = not $ suckedDown (moveJelly jelly mvnt) table
