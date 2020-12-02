module Main where

import Lib (day1, day2)

main :: IO ()
main = do
    numbers <- fmap (read :: String -> Int) . words <$> getContents
    print $ day2 numbers