import Data.Maybe
import Data.List


day4 = length . removable . lines

day4_2 = loop 0 . lines
	where
	loop count grid
		| removed == 0 = count
		| otherwise = loop (count + removed) grid'
		where
		r = removable grid
		(grid', removed) = (update, length r)
		h = length (head grid)
		w = length grid
		update = [[if (x,y) `elem` r then '.' else grid!!x!!y | x <- [0..w-1]] | y <- [0..h-1]]

removable grid = [(x,y) | x <- [0..w-1], y <- [0..h-1], grid!!x!!y == '@', adj x y < 4]
	where
	h = length (head grid)
	w = length grid

	adj x y = sum [1 | ax <- [-1..1], ay <- [-1..1], ax /= 0 || ay /= 0, f (x+ax) (y+ay)]
		where
		f x y = maybe False (=='@') (grid!?x >>= (!?y))


main = readFile "day4_input" >>= mapM_ print . sequence [day4, day4_2]

