module Lib
    ( day1
    ) where

import Debug.Trace (trace)
import Data.List (sort)

day1 :: [Int] -> Maybe Int
day1 l = let l' = sort l
             r = reverse l'
         in search l' r
    where search [] _ = Nothing
          search _ [] = Nothing
          search (l:ls) (r:rs) 
             | l + r == 2020 = Just $ l * r
             | r > 2020 - l  = search (l:ls) rs
             | otherwise     = search ls (r:rs)