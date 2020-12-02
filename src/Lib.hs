module Lib
    ( day1,
      day2
    ) where

import Debug.Trace (trace)
import Data.List (sort)
import qualified Data.Set as S (empty, insert, member, Set)

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

day2 :: [Int] -> Maybe Int
day2 l = let s = S.empty :: S.Set (Int, Int)
         in search l l s
    where search [] _ _ = Nothing
          search (x:xs) [] s = search xs xs s
          search (x:xs) (y:ys) s 
            | S.member (x, 2020 - x - y) s = Just $ x * y * (2020 - x - y)
            | S.member (2020 - x - y, y) s = Just $ x * y * (2020 - x - y)
            | otherwise = search (x:xs) ys $ S.insert (x, y) s
