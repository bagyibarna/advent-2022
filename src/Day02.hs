module Day02 (part1, part2) where

import Data.Maybe (fromJust)
import Data.List (find)

data Shape = Rock | Paper | Scissors
  deriving (Eq)

data Match = Match Shape Shape 
  deriving (Eq)

relations = [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)]

beat shape = fromJust $ lookup shape relations
lose shape = fst . fromJust $ find ((==) shape . snd) relations
draw = id

beats theirs mine = mine == (beat theirs)

readMatch chooser [left, ' ', right] = do
  theirs <- case left of
    'A' -> Just Rock
    'B' -> Just Paper
    'C' -> Just Scissors
    _ -> Nothing
  ours <- chooser theirs right
  return $ Match theirs ours
readMatch _ _ = Nothing

readChoice _ 'X' = Just Rock
readChoice _ 'Y' = Just Paper
readChoice _ 'Z' = Just Scissors
readChoice _ _   =  Nothing

calculateChoice theirs strat = strategy <*> (Just theirs)
  where
    strategy = case strat of 
      'X' -> Just lose
      'Y' -> Just draw
      'Z' -> Just beat
      _   -> Nothing

matchScore :: Match -> Int
matchScore (Match theirs mine) = outcomePoint + shapePoint mine 
  where
    shapePoint Rock = 1
    shapePoint Paper = 2
    shapePoint Scissors = 3
    outcomePoint
      | beats theirs mine = 6
      | beats mine theirs = 0
      | otherwise        = 3

totalScore myReader content = do
  sum <$> (sequence . map readLine $ lines content)
  where
    readLine line = matchScore <$> (readMatch myReader line)

part1 = show . totalScore readChoice
part2 = show . totalScore calculateChoice
