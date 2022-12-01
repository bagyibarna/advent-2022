module Main (main) where

import System.Environment (getArgs)
import qualified Day01

implementation :: String -> Maybe (String -> String, String -> String)
implementation "01" = Just (Day01.part1, Day01.part2)
implementation _ = Nothing

main = do 
  day <- head <$> getArgs 
  execDay day =<< readFile ("input" ++ day ++ ".txt")
  where
    execDay day input = case implementation day of 
      Just (part1, part2) -> do
        putStrLn ("part 1: " ++ part1 input)
        putStrLn ("part 2: " ++ part2 input)
      Nothing -> putStrLn "invalid day number"