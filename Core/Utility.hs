module Core.Utility
    (
        getRow,
        getColumn,
        insideBounds,
        outBounds,
        fromMaybeToEither,
        (<?>)
    ) where

import Data.List
import Data.Maybe (fromMaybe)

-- info: [1..]
getRow :: [[a]] -> Int -> Maybe [a]
getRow xs i = if isSafe then Just (xs !! (i - 1)) else Nothing
    where
        isSafe = i <= length xs && i > 0

getColumn :: [[a]] -> Int -> Maybe [a]
getColumn xs = getRow (transpose xs)

insideBounds :: (Int, Int) -> [[a]] -> Bool
insideBounds pos m = not $ outBounds pos m

outBounds :: (Int, Int) -> [[a]] -> Bool
outBounds (a, b) m = a <= 0 || b <= 0 || a > xa || b > xb
    where 
        matrixShape :: [[a]] -> (Int, Int)
        matrixShape [] = (0, 0)
        matrixShape xs@(x:_) = (length xs, length x)
        (xa, xb) = matrixShape m

-- Convert a Maybe a value to an Either error a value
fromMaybeToEither :: e -> Maybe a -> Either e a
fromMaybeToEither err = maybe (Left err) Right

(<?>) :: e -> Maybe a -> Either e a
s <?> m = fromMaybeToEither s m

infix 8 <?>

