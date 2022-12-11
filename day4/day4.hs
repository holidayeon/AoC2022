module Day4 where

import System.IO ()


splitDelimiter :: String -> Char -> (String, String)
splitDelimiter x delimiter = 
    let (tmp1, tmp2) = break (== delimiter) x in
    (tmp1, drop 1 tmp2)


str2int :: (String, String) -> (String, String) -> ((Int, Int), (Int, Int))
str2int x y =
    ((read (fst x) :: Int, read (snd x) :: Int), (read (fst y) :: Int, read (snd y) :: Int))


containFull :: (Ord a1, Ord a2, Num a3) => ((a1, a2), (a1, a2)) -> a3
containFull (x, y) 
    | (fst x) <= (fst y) && (snd x) >= (snd y) = 1
    | (fst x) >= (fst y) && (snd x) <= (snd y) = 1
    | otherwise = 0


overlap :: (Ord a1, Num a2) => ((a1, a1), (a1, a1)) -> a2
overlap (x, y)
    | (snd x) >= (fst y) && (snd x) <= (snd y) = 1
    | (snd y) >= (fst x) && (snd y) <= (snd x) = 1
    | otherwise = 0


main :: IO ()
main = do
    sectionInput <- readFile "input.txt"

    let sections = lines sectionInput
        splited = map (\(x, y) -> (splitDelimiter x '-', splitDelimiter y '-')) $ map (\x -> splitDelimiter x ',') sections 
        splited2Int = map (\(x, y) -> str2int x y) splited
        checkContains = map containFull splited2Int

        -- Part 2
        checkOverlap = map overlap splited2Int
    
    print $ sum checkContains
    print $ sum checkOverlap
