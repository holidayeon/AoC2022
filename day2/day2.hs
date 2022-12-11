module Day2 where

import System.IO ()

-- Rock Paper Scissors
data Me = X | Y | Z deriving (Show, Read, Enum, Eq)
data You = A | B | C deriving (Show, Read, Enum, Eq)


win :: [(You, Me)]
win = [(C::You, X::Me) , (A::You, Y::Me) , (B::You, Z::Me)]
draw :: [(You, Me)]
draw = [(A::You, X::Me), (B::You, Y::Me), (C::You, Z::Me)]
lose :: [(You, Me)]
lose = [(B::You, X::Me), (C::You, Y::Me), (A::You, Z::Me)]


calculateScore :: (You, Me) -> Int
calculateScore x
    | elem x win = 6 + fromEnum (snd x) + 1 
    | elem x draw = 3 + fromEnum (snd x) + 1 
    | elem x lose = fromEnum (snd x) + 1


calculateScore2 :: (You, Me) -> Int
calculateScore2 (you, res)
    | res == (X::Me) = calculateScore $ (if x == [] then (A, X)::(You, Me) else x!!0) 
    | res == (Y::Me) = calculateScore $ (if y == [] then (A, X)::(You, Me) else y!!0) 
    | res == (Z::Me) = calculateScore $ (if z == [] then (A, X)::(You, Me) else z!!0) 
    where 
        x = filter (\m -> fst m == you) lose
        y = filter (\m -> fst m == you) draw
        z = filter (\m -> fst m == you) win


main :: IO ()
main = do
    -- 1. Read input file
    result <- readFile "input.txt"

    let result2str = lines result
        result2RPS = map (\(x:' ':y) -> (read [x]::You, read y::Me)) result2str

        result2score_v1 = map calculateScore result2RPS
        result2score_v2 = map calculateScore2 result2RPS
             
    print $ sum result2score_v1

    print $ sum result2score_v2
