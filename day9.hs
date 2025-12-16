import Data.List.Split
import Data.List
import Data.Ord
import qualified Data.HashSet as Set
import Data.Hashable


day9 = head . sortBy (comparing Down) . (\a -> [(abs (x1-y1)+1) * (abs (x2-y2)+1) | (x1:x2:[]) <- a, (y1:y2:[]) <- a]) . map (map read . splitOn ",") . lines

day9_2 :: String -> Int
day9_2 input = head $ map fst $ filter foo $ sortBy (comparing Down) [((abs (x1-y1)+1) * (abs (x2-y2)+1), ((x1,x2), (y1,y2))) | (x1:x2:[]) <- points, (y1:y2:[]) <- points, x1 /= x2 && y1 /= y2]
	where
	foo (_, ((x1,x2), (y1,y2))) = all (`Set.member` allowed) [[x,y] | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]
	points = map (map read . splitOn ",") $ lines input
	(minX, maxX, minY, maxY) = (minimum $ map head points, maximum $ map head points, minimum $ map last points, maximum $ map last points)
	greens = nub $ concat [line p1 p2 | (p1,p2) <- zip points (tail $ cycle points)] ++ [(x:y:[]) | x <- [minX..maxX], y <- [minY..maxY], inside points [x,y]]
		where
		inside poly (x:y:[]) = odd $ length $ filter (\((x1:y1:[]),(x2:y2:[])) -> y1 /= y2 && min y1 y2 < y && y <= max y1 y2 && x < x1) $ zip poly (tail $ cycle poly)
		line (x1:y1:[]) (x2:y2:[])
			| x1 == x2 = [[x1, y] | y <- [min y1 y2 .. max y1 y2]]
			| y1 == y2 = [[x, y1] | x <- [min x1 x2 .. max x1 x2]]
	allowed = Set.fromList $ nub $ points ++ greens


main = mapM_ print . sequence [day9, day9_2] =<< readFile "day9_input"

