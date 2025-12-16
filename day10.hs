import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Data.Bits (xor, setBit)


day10 = sum . map minButtons . map (parse . words) . lines
	where
	parse ls = (parseTarget target, map parseButton $ init rest)
		where
		(target:[], rest) = splitAt 1 ls
		parseTarget = foldl' setBit 0 . map fst . filter ((==) '#' . snd) . zip [0..] . drop 1
		parseButton = foldl' setBit 0 . map read . splitOn "," . init . drop 1

	minButtons :: (Int, [Int]) -> Int
	minButtons (target, buttons) = minimum [length subset | subset <- subsequences buttons, (foldl' xor 0 subset) == target]

day10_2 = sum . map minPresses . map parseMachine . lines
	where
	parseMachine line = (target, buttonVectors)
		where
		parts = words line
		target = parseTarget $ last parts
		buttons = map parseButton $ init $ tail parts
		n = length target
		buttonVectors = map (buttonToVector n) buttons

	parseTarget s = map read $ splitOn "," $ init $ drop 1 s
	parseButton "()" = []
	parseButton s = map read $ splitOn "," $ init $ drop 1 s

	buttonToVector n indices = [if i `elem` indices then 1 else 0 | i <- [0..n-1]]

	minPresses :: ([Int], [[Int]]) -> Int
	minPresses (target, buttons) = foo Set.empty [(0, replicate n 0)]
		where
		n = length target
		foo visited [] = error "No solution"
		foo visited ((presses, state):queue)
			| state == target = presses
			| state `Set.member` visited = foo visited queue
			| otherwise = foo (Set.insert state visited) (queue ++ newItems)
			where newItems = [(presses + 1, s) | btn <- buttons, let s = zipWith (+) state btn, all (uncurry (<=)) $ zip s target, s `Set.notMember` visited]


main = mapM_ print . sequence [day10, day10_2] =<< readFile "day10_input"

