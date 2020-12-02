module Main where

import Lib (day1)

main :: IO ()
main = do
    numbers <- fmap (read :: String -> Int) . words <$> getContents
    print $ day1 numbers