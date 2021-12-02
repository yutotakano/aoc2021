import AOC

main = aoc "day2" part1 part2

-- | Move instruction with tuple of (forward distance, down distance)
-- Negative distance forward means backwards, and negative down means move up
data Instruction = Move Int Int deriving (Show, Eq)

instance Read Instruction where
    readsPrec _ ('u':'p':' ':n) = [(Move 0 ((-1) * read n), "")]
    readsPrec _ ('d':'o':'w':'n':' ':n) = [(Move 0 (read n), "")]
    readsPrec _ ('f':'o':'r':'w':'a':'r':'d':' ':n) = [(Move (read n) 0, "")]

instance Semigroup Instruction where
    (Move a b) <> (Move c d) = Move (a + c) (b + d)

instance Monoid Instruction where
    mempty = Move 0 0

part1 :: [String] -> Int
part1 input = a * b
  where
    Move a b = mconcat $ map read input


-- For the second type of instruction, we need to maintain state of the aim
-- direction, so a Monoid won't really work.
data Instruction2 = Forward Int | ShiftAim Int deriving (Show, Eq)

instance Read Instruction2 where
    readsPrec _ ('d':'o':'w':'n':' ':n) = [(ShiftAim (read n), "")]
    readsPrec _ ('u':'p':' ':n) = [(ShiftAim ((-1) * read n), "")]
    readsPrec _ ('f':'o':'r':'w':'a':'r':'d':' ':n) = [(Forward (read n), "")]

part2 :: [String] -> Int
part2 input = uncurry (*) $ execInstr2 (0, 0) 0 $ map read input

execInstr2 :: (Int, Int) -> Int -> [Instruction2] -> (Int, Int)
execInstr2 (x, y) aim [] = (x, y)
execInstr2 (x, y) aim (Forward n : xs) = execInstr2 (x + n, y + (aim * n)) aim xs
execInstr2 (x, y) aim (ShiftAim n : xs) = execInstr2 (x, y) (aim + n) xs
