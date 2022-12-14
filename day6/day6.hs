module Day6 where

import System.IO ()
import Data.Set (toList, fromList)


findSignal :: [(Int, Char)] -> [(Int, Char)]
findSignal signal_w_idx = find where
    find = do
        -- Part 1 : take 4
        -- Part 2 : take 14
        let query_w_idx = take 14 signal_w_idx
            characters = map (\x -> [snd x]) query_w_idx
            query = concat characters

        let marker = if (length (toList (fromList query))) == 14 then query_w_idx else drop 1 signal_w_idx

        if (length marker) == 14 then marker else findSignal marker


main :: IO ()
main = do
    signal_input <- readFile "input.txt"

    let signal = take ((length signal_input) - 2) signal_input 
        signal_w_idx = zip [1..length signal] signal

        marker = findSignal signal_w_idx

    print marker
    