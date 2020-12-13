module Lib
    ( day1_1
    , day1_2
    , day2_1
    , day2_2
    , day3_1
    , day3_2
    , day4_1
    , day4_2
    , day5_1
    , day5_2
    ) where

import Debug.Trace (trace, traceShow)
import Data.List (sort)
import Data.Char  
import Data.List.Split (splitOn)
import qualified Data.Set as S (empty, insert, member, notMember, Set, fromList)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Text.ParserCombinators.ReadP as TP
import Text.Read (readMaybe)

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

t = "a:aa b:bb\nc:cc d:dd\n\ne:ee f:ff\n\ng:gg h:hh\ni:ii\nj:jj"
day4_1 :: String -> Int
day4_1 contents = let fieldLists = map (map (head . splitOn ":") . words . unwords) $ splitOn [""] $ lines contents
                  in length $ filter (\fl -> length fl == 8 || (length fl == 7 && "cid" `notElem` fl)) fieldLists

day4_2 :: String -> Int
day4_2 contents = let fieldLists = map (M.fromList . map ((\[a,b] -> (a, b)) . splitOn ":") . words . unwords) $ splitOn [""] $ lines contents
                  in length $ filter validate fieldLists
  where validate fl
          | length fl <= 6 = False
          | length fl == 7 && "cid" `M.member` fl = False
          | not $ validateYear 1920 2002 (M.lookup "byr" fl) = False
          | not $ validateYear 2010 2020 (M.lookup "iyr" fl) = False
          | not $ validateYear 2020 2030 (M.lookup "eyr" fl) = False
          | not $ validateHeight (M.lookup "hgt" fl)         = False
          | not $ validateHairColor (M.lookup "hcl" fl)      = False
          | not $ validateEyeColor (M.lookup "ecl" fl)       = False
          | not $ validatePassport (M.lookup "pid" fl)       = False
          | otherwise = True

        validateYear min max (Just byr) = case (readMaybe byr :: Maybe Int) of
                                   Just y  -> y >= min && y <= max
                                   Nothing -> False
        validateYear _   _   Nothing    = False

        validateHeight (Just hs) = let (lstr, t) = span isDigit hs
                                       maybeL = readMaybe lstr :: Maybe Int
                                   in case maybeL of
                                     Just l -> case t of
                                       "cm" -> l >= 150 && l <= 193
                                       "in" -> l >= 59 && l <= 76
                                       _    -> False
                                     _      -> False
        validateHeight Nothing   = False
        
        validateHairColor (Just c) = length c == 7 && head c == '#' && all isHexDigit (tail c)
        validateHairColor Nothing  = False
        
        validateEyeColor (Just c) = c `S.member` S.fromList ["amb","blu","brn","gry","grn","hzl","oth"]
        validateEyeColor Nothing  = False

        validatePassport (Just p) = length p == 9 && all isDigit p
        validatePassport Nothing  = False

day5_1 :: [String] -> Int
day5_1 passes = maximum $ map getSeatNum passes
  where getSeatNum pass = let row = readBinStr 0 'F' 'B' $ take 7 pass
                              col = readBinStr 0 'L' 'R' $ (take 3 . drop 7) pass
                          in row * 8 + col
        readBinStr :: Int -> Char -> Char -> [Char] -> Int
        readBinStr acc _ _ [] = acc
        readBinStr acc c0 c1 (c:cs) =
         if c == c0
          then
            readBinStr (acc * 2) c0 c1 cs
          else
            readBinStr (acc * 2 + 1) c0 c1 cs

day5_2 :: [String] -> Int
day5_2 passes = let ids = S.fromList $ map getSeatNum passes
                    mx = maximum ids
                in head $ filter (\x -> x `S.notMember` ids && (x-1) `S.member` ids && (x+1) `S.member` ids) [0..mx]
  where getSeatNum pass = let row = readBinStr 0 'F' 'B' $ take 7 pass
                              col = readBinStr 0 'L' 'R' $ (take 3 . drop 7) pass
                          in row * 8 + col
        readBinStr :: Int -> Char -> Char -> [Char] -> Int
        readBinStr acc _ _ [] = acc
        readBinStr acc c0 c1 (c:cs) =
         if c == c0
          then
            readBinStr (acc * 2) c0 c1 cs
          else
            readBinStr (acc * 2 + 1) c0 c1 cs
