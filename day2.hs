import Data.List.Split


day2 = sum . concatMap (filter (repeats . show) . (\[a,b] -> [a..b-1]) . map read . splitOn "-") . splitOn ","
	where
	repeats s = even (length s) && ((\(a,b) -> a == b) (splitAt (length s `div` 2) s ))

day2_2 = sum . concatMap (filter (repeats . show) . (\[a,b] -> [a..b-1]) . map read . splitOn "-") . splitOn ","
	where
	repeats s = let l = length s in any check [k | k <- [1..l-1], l `mod` k == 0, l `div` k >= 2]
		where
		check k = all (== (take k s)) $ chunksOf k s


main = readFile "day2_input" >>= mapM_ print . sequence [day2, day2_2]

