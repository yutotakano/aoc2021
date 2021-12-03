import Data.Char
import AOC
import Debug.Trace

main = aoc "day3" part1 part2

binToDec :: [Int] -> Int
binToDec input = binToDec' 0 $ reverse input
  where
    binToDec' power [] = 0
    binToDec' power (lsb:revInput) = 2^power * lsb + binToDec' (power + 1) revInput

flipBin :: [Int] -> [Int]
flipBin = map (flip mod 2 . (+) 1)

sumEachIndex :: [[Int]] -> [Int]
sumEachIndex = foldr1 (\x y -> map (uncurry (+)) $ zip x y)

oneAboveHalf :: Int -> Int -> Int
oneAboveHalf len x
    | x > (len `div` 2) = 1
    | even len && x >= (len `div` 2) = 1
    | otherwise = 0


san input = map (map digitToInt) input
gamma input = map (oneAboveHalf (length input)) . sumEachIndex $ input
epsilon = flipBin . gamma

part1 input = binToDec (gamma $ san input) * binToDec (epsilon $ san input)

filterMatches :: (Show a, Eq a) => ([[a]] -> [a]) -> [[a]] -> [a]
filterMatches _ [x] = x
filterMatches f input = x : (filterMatches f . map tail $ filtered)
  where
    (x:xs) = f input
    filtered = filter ((== x) . head) $ input

part2 input = binToDec oxy * binToDec co2
  where
    oxy = filterMatches gamma $ san input
    co2 = filterMatches epsilon $ san input

