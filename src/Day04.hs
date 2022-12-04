module Day04 (part1, part2) where

import Data.List.Split (splitOn)

assignments :: String -> [((Int, Int), (Int, Int))]
assignments = map (toTuple . map (toTuple . map read . splitOn ['-']) . splitOn [',']) . lines
  where
    toTuple [from, to] = (from, to)
    toTuple _ = error "invalid input"

subranges left right = contains left right || contains right left
  where
    contains (leftStart, leftEnd) (rightStart, rightEnd) = leftStart <= rightStart && leftEnd >= rightEnd

overlap (leftStart, leftEnd) (rightStart, rightEnd) = leftBetween || rightBetween
  where
    leftBetween = between leftStart rightStart rightEnd || between leftEnd rightStart rightEnd
    rightBetween = between rightStart leftStart leftEnd || between rightEnd leftStart leftEnd
    between reference small large = reference >= small && reference <= large

part1 = show . length . filter (uncurry subranges) . assignments
part2 = show . length . filter (uncurry overlap) . assignments