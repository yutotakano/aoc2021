module AOC where

import System.Environment

aoc day part1 part2 = do
    args <- getArgs
    case args of
        ["1"] -> do
            input <- readFile $ day <> "part1.input.txt"
            print $ part1 $ lines input
        ["2"] -> do
            input <- readFile $ day <> "part2.input.txt"
            print $ part2 $ lines input
        _ -> putStrLn "Usage: ./<executable> <1|2>"
