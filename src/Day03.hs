module Day03 (part1, part2) where

import Data.Char (ord)
import Data.List.Split (chunksOf)
import qualified Data.Set as Set

getCompartments l = (Set.fromList left, Set.fromList right)
  where
    (left, right) = splitAt ((length l) `div` 2) l

getPriosForElf leftSet rightSet = sum . map priority . Set.toList $ Set.intersection leftSet rightSet

getBadgeOfGroup = Set.elemAt 0 . foldl1 Set.intersection . map Set.fromList
 
priority :: Char -> Int
priority ch
  | ch >= 'A' && ch <= 'Z' = ord ch - ord 'A' + 27
  | ch >= 'a' && ch <= 'z' = ord ch - ord 'a' + 1
  | otherwise = error "invalid input" 

part1 = show . sum . map (uncurry getPriosForElf . getCompartments) . lines
part2 =  show . sum . map (priority . getBadgeOfGroup) . chunksOf 3  . lines