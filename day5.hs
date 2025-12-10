import Data.Bifunctor (bimap)
import Data.List (sort)


day5 input = length $ filter (\id -> any (fresh id) (map parse ranges)) $ map read ids
	where
	(ranges, (_:ids)) = break null (lines input)
	fresh id (lo, hi) = lo <= id && id <= hi

day5_2 input = sum $ map (\(a,b) -> b-a+1) $ merge $ sort $ map parse ranges
	where
	(ranges, _) = break null (lines input)

	merge [] = []
	merge [x] = [x]
	merge ((a1, b1):(a2, b2):ls)
		| b1 >= a2-1 = merge ((a1, max b1 b2):ls)
		| otherwise = (a1, b1) : merge ((a2, b2):ls)

parse = bimap read (read . tail) . break (=='-')


main = mapM_ print . sequence [day5, day5_2] =<< readFile "day5_input"

