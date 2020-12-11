module Lib
    ( day1_1
    , day1_2
    , day2_1
    , day2_2
    , day3_1
    , day3_2
    -- , day4_1
    ) where

import Debug.Trace (trace)
import Data.List (sort)
import qualified Data.Set as S (empty, insert, member, Set)
import qualified Data.Text as T
import qualified Text.ParserCombinators.ReadP as TP

day1_1 :: [Int] -> Maybe Int
day1_1 l = let l' = sort l
               r = reverse l'
           in search l' r
    where search [] _ = Nothing
          search _ [] = Nothing
          search (l:ls) (r:rs) 
             | l + r == 2020 = Just $ l * r
             | r > 2020 - l  = search (l:ls) rs
             | otherwise     = search ls (r:rs)

day1_2 :: [Int] -> Maybe Int
day1_2 l = let s = S.empty :: S.Set (Int, Int)
           in search l l s
    where search [] _ _ = Nothing
          search (x:xs) [] s = search xs xs s
          search (x:xs) (y:ys) s 
            | S.member (x, 2020 - x - y) s = Just $ x * y * (2020 - x - y)
            | S.member (2020 - x - y, y) s = Just $ x * y * (2020 - x - y)
            | otherwise = search (x:xs) ys $ S.insert (x, y) s

day2_1 :: [(Int, Int, Char, String)] -> Int
day2_1 entries = length $ filter validPW entries
  where validPW (mn, mx, c, pw) = let count = length $ filter (\c' -> c == c') pw
                                  in count >= mn && count <= mx

day2_2 :: [(Int, Int, Char, String)] -> Int
day2_2 entries = length $ filter validPW entries
  where validPW (pos1, pos2, c, pw) = let c1 = gc pw pos1
                                          c2 = gc pw pos2
                                      in (c1 == c && c2 /= c) || (c1 /= c && c2 == c)
        gc []     pos = ' '
        gc (c:cs) 1   = c
        gc _      0   = ' '
        gc (c:cs) pos = gc cs (pos - 1)

day3_1 :: [String] -> Int
day3_1 rows = let rows' = fmap T.pack rows
              in f rows' 0 (T.length $ head rows') 0
  where
    f []     _ _ c = c 
    f (r:rs) x w c = case nth x r of
                       '.' -> f rs ((x + 3) `mod` w) w c
                       '#' -> f rs ((x + 3) `mod` w) w (c + 1)
    nth i = T.head . T.drop i 

day3_2 :: [String] -> Int
day3_2 rows = let rows' = fmap T.pack rows
              in product $ fmap (uncurry $ countTrees rows' 0 (T.length $ head rows') 0) [(1,1),(3,1),(5,1),(7,1),(1, 2)]
  where
    countTrees []     _ _ c _    _ = c 
    countTrees (r:rs) x w c xinc yinc = let rs' = drop (yinc - 1) rs
                                            x' = (x + xinc) `mod` w
                                        in 
                                          case nth x r of
                       '.' -> countTrees rs' x' w c xinc yinc
                       '#' -> countTrees rs' x' w (c + 1) xinc yinc
    nth i = T.head . T.drop i

-- day4_1 :: String -> Int
-- day4_1 contents = TP.readP_to_S
--   where
