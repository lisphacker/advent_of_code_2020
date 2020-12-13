module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Lib

main1 :: IO ()
main1 = do
    numbers <- fmap (read :: String -> Int) . words <$> getContents
    print $ day1_2 numbers

main2 :: IO ()
main2 = do
    entries <- fmap f . lines <$> getContents
    print $ day2_2 entries
    where f s = let ws = words s
                    rangeStrs = splitOn "-" $ ws !! 0
                    mn = (read $ rangeStrs !! 0) :: Int
                    mx = (read $ rangeStrs !! 1) :: Int
                in (mn, mx, (ws !! 1) !! 0, ws !! 2)

main3 :: IO ()
main3 = do
    rows <- lines <$> getContents
    print $ day3_2 rows

main4 :: IO ()
main4 = do
    contents <- getContents
    print $ day4_2 contents

main5 :: IO ()
main5 = do
    rows <- lines <$> getContents
    print $ day5_2 rows

main :: IO ()
main = main5
