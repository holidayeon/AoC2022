module Day1 where

import System.IO ()
import Data.List (elemIndices, sort)


sumOverIndex :: [Int] -> [Int] -> [Int]
sumOverIndex x zeroidx =
    map (\(i, elem) -> (+) 1 $ sum $ drop ((0:zeroidx) !! (i-1)) (take elem x)) 
    (zip [1..(length zeroidx)] (zeroidx ++ [length x]))


main :: IO ()
main = do
    -- 1. Read input File 
    calories <- readFile "input.txt"

    -- 2. Split by lines
    let calories2str = lines calories
        -- 3. String list to Int list and if "" (delimiter) then -1 impossible calorie
        calories2int::[Int] = map (\x -> if x /= "" then read x else (-1)) calories2str
        -- 4. Find indices when element is (-1) in calories2int
        calories_of_idx = elemIndices (-1) calories2int
        -- 5. Sum calories between -1 indices
        calories_sum = sumOverIndex calories2int calories_of_idx 

    -- 6. Top 1 sum of calories
    print $ maximum calories_sum

    -- 7. Sum top 3 sum of calories 
    print $ sum $ take 3 $ reverse $ sort calories_sum
 