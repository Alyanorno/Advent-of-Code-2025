import Data.List
import Data.Maybe (listToMaybe)


day6 = sum . map (calc . reverse) . transpose . map words . lines
	where
	calc ("+":ls) = sum $ map read ls
	calc ("*":ls) = product $ map read ls

day6_2 =  sum . map calc . split . concatMap reverse . transpose . lines
	where
	calc ('+':ls) = sum $ readMany ls
	calc ('*':ls) = product $ readMany ls

	readMany = unfoldr $ listToMaybe . concatMap reads . tails . reverse

	split (l:ls) = (l:x) : split ls'
		where (x, ls') = span (not . (flip elem "+*")) ls
	split [] = []


main = mapM_ print . sequence [day6, day6_2] =<< readFile "day6_input"

