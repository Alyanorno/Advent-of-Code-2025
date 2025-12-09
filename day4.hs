import Data.Maybe
import Data.List


day4 = length . removable . lines

day4_2 = sum . map length . takeWhile (not . null) . map removable . iterate loop . lines
	where loop g = remove (removable g)
		where remove r = [[if (x,y) `elem` r then '.' else g!!x!!y | x <- [0..length g-1]] | y <- [0..length (head g)-1]]

removable grid = [(x,y) | x <- [0..length (head grid)-1], y <- [0..length (head grid)-1], grid!!x!!y == '@', adj x y < 4]
	where adj x y = sum [1 | ax <- [-1..1], ay <- [-1..1], (ax,ay) /= (0,0), f (x+ax) (y+ay)]
		where f x y = maybe False (=='@') (grid!?x >>= (!?y))


main = readFile "day4_input" >>= mapM_ print . sequence [day4, day4_2]
