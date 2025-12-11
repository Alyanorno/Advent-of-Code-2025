import Data.List.Split
import Data.List
import Data.Ord


day9 = head . sortBy (comparing Down) . (\a -> [(abs (x1-y1)+1) * (abs (x2-y2)+1) | (x1:x2:[]) <- a, (y1:y2:[]) <- a]) . map (map read . splitOn ",") . lines

day9_2 _ = 0


main = mapM_ print . sequence [day9, day9_2] =<< readFile "day9_input"

