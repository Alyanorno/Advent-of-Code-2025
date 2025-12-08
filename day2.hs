import Data.List.Split
import Data.List


day2 = sum . concatMap (filter (repeats . show) . (\[a,b] -> [a..b]) . map read . splitOn "-") . splitOn ","
	where
	repeats s = even (length s) && ((\(a,b) -> a == b) (splitAt (length s `div` 2) s ))

day2_2 = sum . concatMap (filter (repeats . show) . (\[a,b] -> [a..b]) . map read . splitOn "-") . splitOn ","
	where
	repeats s = let l = length s in any ((\(x:xs) -> all (== x) xs) . flip chunksOf s) [k | k <- [1..l], l `mod` k == 0, l `div` k >= 2]


main = readFile "day2_input" >>= mapM_ print . sequence [day2, day2_2]

