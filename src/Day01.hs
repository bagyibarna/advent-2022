module Day01 (part1, part2) where

import Data.List (sortBy)
import Data.List.Split (splitOn)

caloriesPerElf :: String -> [Int]
caloriesPerElf = map (sum . map read) . (splitOn [""]) . lines

part1 = show . maximum . caloriesPerElf
part2 = show . sum . take 3 . sortBy (flip compare) . caloriesPerElf