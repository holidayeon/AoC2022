module Day3 where

import System.IO ()
import Data.List (elemIndex, splitAt)
import Data.Maybe (fromMaybe)


splitCompartment :: [a] -> Int -> ([a], [a])
splitCompartment x idx = 
    (drop idx x, take idx x)

intersect :: (Foldable t, Eq a) => t a -> [a] -> [a]
intersect a b = filter (\x -> elem x a) b

splitEvery :: Int -> [a] -> [[a]]
splitEvery n lst 
    | null lst = []
    | otherwise = take n lst:splitEvery n (drop n lst) 

intersectTriple :: (Eq a, Foldable t1, Foldable t2) => t2 a -> [a] -> t1 a -> [a]
intersectTriple a b c = filter (\y -> elem y c) $ filter (\x -> elem x a) b


main :: IO ()
main = do

    items <- readFile "input.txt"

    let items2str = lines items
        -- Part 1
        midIndices = map (\x -> div (length x) 2) items2str
        splitedItems = map (\(x, i) -> splitCompartment x i) (zip items2str midIndices)
        commonItems = map (\(a, b) -> head $ intersect a b) splitedItems

        priorities = ['a'..'z'] ++ ['A'..'Z']
        commonPriorities = map (\x -> ((fromMaybe 0) (elemIndex x priorities)) + 1) commonItems

        -- Part 2 
        groupItems = splitEvery 3 items2str
        commonGroupItems = map (\[a, b, c] -> head $ intersectTriple a b c) groupItems
        commonGroupPriorities = map (\x -> ((fromMaybe 0) (elemIndex x priorities)) + 1) commonGroupItems


    print $ sum commonPriorities
    print $ sum commonGroupPriorities
