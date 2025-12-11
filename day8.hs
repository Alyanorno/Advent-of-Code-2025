import Data.List (sort, sortBy)
import Data.List.Split
import Data.Ord


nrPairs = 1000

day8 = product . take 3 . sortBy (comparing Down) . map length . foldl' merge start . take nrPairs . sort . calc . zip [0..] . map (map read . splitOn ",") . lines
	where
	calc indexed = [(distance x y, xi, yi) | (xi, x) <- indexed, (yi, y) <- indexed, xi < yi]
	merge ls (_, xi, xy)
		| x == y = ls
		| otherwise = (x ++ y) : filter (`notElem` [x, y]) ls
		where
		x = find xi ls
		y = find xy ls

day8_2 = (\indexed -> step indexed start $ sort $ calc indexed) . zip [0..] . map (map read . splitOn ",") . lines
	where
	calc indexed = [(distance x y, xi, yi) | (xi, x) <- indexed, (yi, y) <- indexed, xi < yi]
	step indexed ls ((_, i, j) : rest)
		| x == y = step indexed ls rest
		| length new == 1 = product $ map (head . snd . (indexed !!)) [i, j]
		| otherwise = step indexed new rest
		where
		x = find i ls
		y = find j ls
		new = (x ++ y) : filter (`notElem` [x, y]) ls

start = map (:[]) [0..nrPairs-1]
distance x y = sum $ map (^2) $ zipWith (-) x y
find i = head . filter (elem i)


main = mapM_ print . sequence [day8, day8_2] =<< readFile "day8_input"

