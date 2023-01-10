module Core.Base (
    HardRegion(..),
    Region(..),
    World(..),
    Jelly(..),
    Boardgame,
    Cursor,
    Command(..),
    fromCharToHardRegion,
    fromHardRegionToChar,
    fromCharToRegion,
    fromRegionToChar,
    getShape,
    setShape,
    getJelly,
    setJelly,
    getCursor,
    setCursor,
    getBoardgame,
    setBoardgame,
    grepRegionsSafely,
) where

import Control.Monad
import Data.List
import Data.Char
import Debug.Trace

import Core.Utility
    (
       insideBounds,
       outBounds
    )

data HardRegion = HardGoal | HardSolid | HardIce | HardHole | HardJelly deriving (Show, Eq)
data Region = Goal | Solid | Ice | Hole deriving (Show, Eq)

type Boardgame  = [[Region]]

newtype Jelly = Jelly Shape deriving Show
type Cursor = (Int, Int)
type Shape = (Int, Int, Int)

data World = World {
    jelly :: Jelly,
    cursor :: Cursor,
    meta :: [(Int, Int)],
    boardGame :: Boardgame
}

-- "| " ++  intercalate " | " (foldr (\c acc -> [c]:acc) [] "hola") ++ " |"

instance Show World where
  show (World j c _ board) = beautifier $ brusher base points
    where
      base = (map . map) fromRegionToChar board
      points = supportPoints board (c, j)

      brush :: [String] -> (Int, Int) -> [String]
      brush canvas (j, i) =
                        let
                          ya = take (pred j) canvas
                          yb  = drop j canvas
                          xa = take (pred i) (canvas !! pred j)
                          xb = drop i (canvas !! pred j)
                          line = xa ++ (fromHardRegionToChar HardJelly : xb)
                        in ya ++ (line : yb)

      brusher :: [String] -> [(Int, Int)] -> [String]
      brusher [] _ = []
      brusher canvas [] = canvas
      brusher canvas (p:ps) = brusher (brush canvas p) ps

      beautifier :: [String] -> String
      beautifier b =
        let lines = map (\line -> "| " ++  intercalate " | " (map (: []) line) ++ " | ") b
            n = length $ transpose lines
        in  replicate (n - 1) '-'
            ++
            "\n"
            ++
            intercalate "\n" lines
            ++
            "\n"
            ++
            replicate (n - 1) '-'

data Command = Left_ | Right_ | Up_ | Down_ deriving Eq

fromCharToHardRegion :: Char -> Maybe HardRegion
fromCharToHardRegion '0' = Just HardHole
fromCharToHardRegion '1' = Just HardGoal
fromCharToHardRegion '2' = Just HardIce
fromCharToHardRegion '3' = Just HardSolid
fromCharToHardRegion 'J' = Just HardJelly
fromCharToHardRegion _ = Nothing

fromHardRegionToChar :: HardRegion -> Char
fromHardRegionToChar HardHole = '0' 
fromHardRegionToChar HardGoal = '1'
fromHardRegionToChar HardIce = '2'
fromHardRegionToChar HardSolid = '3'
fromHardRegionToChar HardJelly = 'J'

fromCharToRegion :: Char -> Maybe Region
fromCharToRegion '0' = Just Hole
fromCharToRegion '1' = Just Goal
fromCharToRegion '2' = Just Ice
fromCharToRegion c
    | c == '3' || c == 'J' = Just Solid
    | otherwise = Nothing

fromRegionToChar :: Region -> Char
fromRegionToChar Hole = '0' 
fromRegionToChar Goal = '1'
fromRegionToChar Ice = '2'
fromRegionToChar Solid = '3'

getShape :: Jelly -> Shape
getShape (Jelly shape) = shape

setShape :: Shape -> Jelly -> Jelly
setShape s' _ = Jelly s'

getJelly :: World -> Jelly
getJelly = jelly
 
setJelly :: Jelly -> World -> World
setJelly j (World _ c board meta) = World j c board meta
                               
getCursor :: World -> Cursor
getCursor = cursor

setCursor :: Cursor -> World -> World
setCursor c (World j _ m b) = World j c m b

getBoardgame :: World -> Boardgame
getBoardgame = boardGame 

setBoardgame :: Boardgame -> World -> World 
setBoardgame b (World j c m _) = World j c m b

getMeta :: World -> [(Int, Int)] 
getMeta = meta

setMeta :: [(Int, Int)] -> World -> World
setMeta m (World j c _ b) = World j c m b

-- warning: may throw out of range exceptions
grepRegions :: Boardgame -> [(Int, Int)] -> [Region]
grepRegions board = map (\(a, b) -> board !! a !! b) 

grepRegionsSafely :: Boardgame -> [(Int, Int)] -> [Region]
grepRegionsSafely board xs = grepRegions board $ filter (`insideBounds` board) xs

rotate :: Command -> Jelly -> Jelly
rotate c j
    | c == Left_ || c == Right_ = setShape (z, y, x) j
    | otherwise = setShape (x, z, y) j
    where
        (x, y, z) = getShape j

updateCursor :: Command -> (Cursor, Jelly) -> Cursor
updateCursor c ((a, b), j)
    | c == Left_ = (a, b - z)
    | c == Right_ = (a, b + z)
    | c == Up_ = (a - z, b)
    | otherwise = (a + z, b)
    where
        (_, _, z) = getShape j

virtualSupportPoints :: (Cursor, Jelly) -> [(Int, Int)]
virtualSupportPoints ((a, b), Jelly (x, y, z)) =
    do
        j <- [a..(a + (y - 1))]
        i <- [b..(b + (x - 1))]
        return (j, i)

supportPoints :: Boardgame -> (Cursor, Jelly) -> [(Int, Int)]
supportPoints board pack = filter (`insideBounds` board) (virtualSupportPoints pack)

supportRegions :: Boardgame -> (Cursor, Jelly) -> [Region]
supportRegions board pack = grepRegionsSafely board $ filter (`insideBounds` board) (virtualSupportPoints pack)

aboveGoal :: Boardgame -> (Cursor, Jelly) -> Bool
aboveGoal board pack = aboveRegion board pack Goal

aboveIce :: Boardgame -> (Cursor, Jelly) -> Bool
aboveIce board pack = aboveRegion board pack Ice

aboveHole :: Boardgame -> (Cursor, Jelly) -> Bool
aboveHole board pack = aboveRegion board pack Hole

aboveRegion :: Boardgame -> (Cursor, Jelly) -> Region -> Bool
aboveRegion board (csr, j) r = r `elem` grepRegionsSafely board sp
    where
        sp = supportPoints board (csr, j)

overflowAroundEdges :: World -> Bool
overflowAroundEdges (World jelly cursor _ board) = any (`outBounds` board) (virtualSupportPoints (cursor, jelly))

