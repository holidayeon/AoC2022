module Day5 where

import System.IO ()


execute :: [String] -> [Int] -> [String]
execute crates cmd = 
    let 
        start = crates !! start_idx
        start_idx = (cmd !! 1) - 1
        new_start = take (len - (cmd !! 0)) start
        len = length start
        taken = drop (len - (cmd !! 0)) start
        end = crates !! end_idx
        end_idx = (cmd !! 2) - 1
        new_end = end ++ (reverse taken)
        mid_crate = (take start_idx crates) ++ [new_start] ++ (drop (start_idx + 1) crates)
    in
        (take end_idx mid_crate) ++ [new_end] ++ (drop (end_idx + 1) mid_crate)


executeV2 :: [String] -> [Int] -> [String]
executeV2 crates cmd = 
    let 
        start = crates !! start_idx
        start_idx = (cmd !! 1) - 1
        new_start = take (len - (cmd !! 0)) start
        len = length start
        taken = drop (len - (cmd !! 0)) start
        end = crates !! end_idx
        end_idx = (cmd !! 2) - 1
        new_end = end ++ taken
        mid_crate = (take start_idx crates) ++ [new_start] ++ (drop (start_idx + 1) crates)
    in
        (take end_idx mid_crate) ++ [new_end] ++ (drop (end_idx + 1) mid_crate)



main :: IO ()
main = do
    cranesInput <- readFile "input.txt"

    let cranesTemp = lines cranesInput
        (_, commands) = break (== "") cranesTemp
        crate = ["PFMQWGRT", "HFR", "PZRVGHSD", "QHPBFWG", "PSMJH", "MZTHSRPL", "PTHNML", "FDQR", "DSCNLPH"]

        cmd = map words $ tail commands
        cmd_digit = map (\x -> [(read (x!!1)) :: Int, (read (x!!3)) :: Int, (read (x!!5)) :: Int]) $ cmd

        result = foldl (\acc x -> (execute (head acc) x):acc) [crate] cmd_digit

        -- Part 2 
        result_v2 = foldl (\acc x -> (executeV2 (head acc) x):acc) [crate] cmd_digit

    print $ [last x | x <- head result]
    print $ [last x | x <- head result_v2]
    