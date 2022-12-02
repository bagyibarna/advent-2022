module Main (main) where

import System.Environment (getArgs)
import qualified Day01
import qualified Day02

days = [
    ("01", (Day01.part1, Day01.part2)),
    ("02", (Day02.part1, Day02.part2))
  ]

execDay day = case lookup day days of 
  Just (part1, part2) -> do
    input <- readFile ("input" ++ day ++ ".txt")
    putStrLn ("part 1: " ++ part1 input)
    putStrLn ("part 2: " ++ part2 input)
  Nothing -> putStrLn $ "invalid day number, choose one of: " ++ (show $ map fst days)

main = execDay =<< head <$> getArgs
