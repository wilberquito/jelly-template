module Core.Virtualizer 
    (
        mkWorld
    ) where

import Control.Monad
import Text.Read
import Data.List
import Data.Maybe
import Debug.Trace

import Core.Base 
    (
        HardRegion(..),
        Region(..),
        World(..),
        Jelly(..),
        Boardgame,
        Cursor,
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
        setBoardgame
    )

import Core.Utility
    (
        getRow,
        getColumn,
        (<?>)
    )

data VirtualizationError =
  JellyError String |
  CursorError String |
  BoardgameError String |
  MetaCoordsError String
  deriving (Show)

type RawFileContent = [String]

-- Example of how to trace a value in case it's needed
-- j <- trace "Making Jelly..." (mkJelly rlines)
-- c <- trace "Making cursor..." (mkCursor rlines)
-- gb <- trace "Making game board..." (mkBoardgame rlines)
-- m <- trace "Making meta coordinates..." (mkMetaCoords gb)
mkWorld :: RawFileContent -> Either VirtualizationError World
mkWorld rlines =
    do
        j <- mkJelly rlines
        c <- mkCursor rlines
        gb <- mkBoardgame rlines
        m <- mkMetaCoords gb
        return World {
            jelly = j,
            cursor = c,
            boardGame = gb,
            meta = m 
        }

-- info: sequence = mapM id
mkBoardgame :: RawFileContent -> Either VirtualizationError Boardgame
mkBoardgame [] = Left $ BoardgameError "Unexpected empty list previous virtualization"
mkBoardgame (_:xs) = BoardgameError "Problem parsing chars to regions" <?> virtualizeBoardgame xs
    where
        virtualizeBoardgame rlines =
            let regions = (fmap . fmap) fromCharToRegion rlines
            in sequence $ sequence <$> regions 

mkCursor :: RawFileContent -> Either VirtualizationError Cursor
mkCursor rlines = CursorError "Cursor could'nt be created" <?> getJellyCursor rlines
    where
        getJellyCursor :: [String] -> Maybe Cursor
        getJellyCursor [] = Nothing
        getJellyCursor (_:xs) = virtualizeCursor xs

mkJelly :: RawFileContent -> Either VirtualizationError Jelly
mkJelly rlines =
    do
        h <- getJellyHeight rlines
        (x, y) <- JellyError "Grounded surface not found"
                    <?>
                  getJellyGroundedSurface rlines
        return $ Jelly (x, y, h)
    where
        getJellyHeight :: [String] -> Either VirtualizationError Int
        getJellyHeight [] = Left $ JellyError "Can not virtualize height from empty string"
        getJellyHeight (h:_) = validate (readMaybe h :: Maybe Int)
            where
                validate :: Maybe Int -> Either VirtualizationError Int
                validate Nothing =  Left $ JellyError "Height configuration not found"
                validate (Just h)
                    | h <= 0 = Left $ JellyError "The configured height must be equal or greater to 1"
                    | otherwise = return h

        getJellyGroundedSurface :: [String] -> Maybe SurfaceShape
        getJellyGroundedSurface [] = Nothing
        getJellyGroundedSurface (_:xs) = virtualizeGroundedSurface xs

mkMetaCoords :: Boardgame -> Either VirtualizationError [(Int, Int)]
mkMetaCoords board =
    let xi = concatMap (elemIndices Goal) board
        yi = concatMap (elemIndices Goal) (transpose board)
        coords = map (+1) yi `zip` map (+1) xi
    in case coords of
        [] -> Left $ MetaCoordsError "There is no meta positions in the gameboard"
        xs -> Right xs

type RawWorld = [String]
type SurfaceShape = (Int, Int) 

virtualizeCursor :: RawWorld -> Maybe (Int, Int)
virtualizeCursor xs = computePos (xs `zip` [1..]) 
    where
        computePos :: [(String, Int)] -> Maybe (Int, Int)
        computePos [] = Nothing 
        computePos ((l, j): xs) =
            case xPos l of
                Just i -> return (j, i)
                Nothing -> computePos xs
        
        xPos :: String -> Maybe Int
        xPos xs = 
            let 
                indexed = filter (\(c, _) -> c == fromHardRegionToChar HardJelly) (xs `zip` [1..])
            in case indexed of
                [] -> Nothing
                ((_, i):_) -> return i

virtualizeGroundedSurface :: RawWorld -> Maybe SurfaceShape 
virtualizeGroundedSurface xs = virtualizeCursor xs >>= computeSurface
    where
        computeSurface :: (Int, Int) -> Maybe SurfaceShape
        computeSurface (j, i) =
                do
                    column <- getColumn xs i
                    row <- getRow xs j
                    let x = length $ filter (== fromHardRegionToChar HardJelly) row
                        y = length $ filter (== fromHardRegionToChar HardJelly) column
                    return (x, y)

