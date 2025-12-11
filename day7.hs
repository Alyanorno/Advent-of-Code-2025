import Data.List (elemIndex, partition, nub)
import Data.Maybe
import Data.Ix (inRange)


day7 = fst . (\(l:ls) -> foldl step (0, [fromJust $ elemIndex 'S' l]) $ ls) . lines
	where
	step (total, beams) row = (total + length splits, new)
		where
		(splits, passes) = partition ((=='^') . (row!!)) beams
		new = nub $ passes ++ concatMap adjecent splits
			where adjecent = filter (inRange (0, length row)) . flip map [1, -1] . (+)

day7_2 input = sum $ foldl' step (start columns) [1..length grid-1]
	where
	grid = lines $ input
	columnsLenght = length (head grid)-1
	columns = [0..columnsLenght]
	start = map (fromEnum . (==) (fromJust $ elemIndex 'S' $ head grid))
	step state i = map (total $ grid !! (i-1)) columns
		where
		total last = sum . flip map [left, middle, right] . flip id
			where
			left i
				| i-1 >= 0 && last!!(i-1) == '^' = state!!(i-1)
				| otherwise = 0
			middle i
				| last!!i /= '^' = state!!i
				| otherwise = 0
			right i
				| i+1 < columnsLenght && last!!(i+1) == '^' = state!!(i+1)
				| otherwise = 0


main = mapM_ print . sequence [day7, day7_2] =<< readFile "day7_input"

