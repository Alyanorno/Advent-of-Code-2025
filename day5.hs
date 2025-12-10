import Data.Bifunctor (bimap)
import Data.List (sort)


day5 input = length $ filter (flip any (map parse ranges) . fresh) $ map read ids
	where
	(ranges, (_:ids)) = break null (lines input)
	fresh id (lo, hi) = lo <= id && id <= hi

day5_2 input = sum $ map (\(a,b) -> b-a+1) $ merge $ sort $ map parse ranges
	where
	(ranges, _) = break null (lines input)

	merge (a:b:ls)
		| snd b >= fst b-1 = merge ((fst a, max (snd a) (snd b)):ls)
		| otherwise = a : merge (b:ls)
	merge x = x

parse = bimap read (read . tail) . break (=='-')


main = mapM_ print . sequence [day5, day5_2] =<< readFile "day5_input"

