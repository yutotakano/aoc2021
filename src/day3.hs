import Data.Char
import AOC
import Debug.Trace

main = aoc "day3" part1 part2

type Binary = [Int]

binToDec :: Binary -> Int
binToDec input = binToDec' 0 $ reverse input
  where
    binToDec' power [] = 0
    binToDec' power (lsb:revInput) = 2^power * lsb + binToDec' (power + 1) revInput

flipBin :: Binary -> Binary
flipBin = map (flip mod 2 . (+) 1)

sumEachIndex :: [Binary] -> Binary
sumEachIndex = foldr1 (\x y -> map (uncurry (+)) $ zip x y)

oneAboveHalf :: Int -> Int -> Int
oneAboveHalf len x
    | x > (len `div` 2) = 1
    | even len && x >= (len `div` 2) = 1
    | otherwise = 0

parse :: [String] -> [Binary]
parse input = map (map digitToInt) input

gamma :: [Binary] -> Binary
gamma input = map (oneAboveHalf (length input)) . sumEachIndex $ input

epsilon :: [Binary] -> Binary
epsilon = flipBin . gamma

part1 :: [String] -> Int
part1 input = binToDec (gamma $ parse input) * binToDec (epsilon $ parse input)

filterMatches :: ([Binary] -> Binary) -> [Binary] -> Binary
filterMatches _ [x] = x
filterMatches f input = msb : (filterMatches f . map tail $ filtered)
  where
    msb = head $ f input
    filtered = filter ((== msb) . head) $ input

part2 :: [String] -> Int
part2 input = binToDec oxy * binToDec co2
  where
    oxy = filterMatches gamma $ parse input
    co2 = filterMatches epsilon $ parse input

