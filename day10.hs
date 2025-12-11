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

day10_2 _ = 0


main = mapM_ print . sequence [day10, day10_2] =<< readFile "day10_input"

