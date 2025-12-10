import Data.List
import Data.Maybe (listToMaybe)


day6 = sum . map (calc . reverse) . transpose . map words . lines
	where
	calc ("+":ls) = sum $ map read ls
	calc ("*":ls) = product $ map read ls

day6_2 =  sum . map calc . split . concatMap reverse . transpose . lines
	where
	calc ('+':ls) = sum $ numbers ls
	calc ('*':ls) = product $ numbers ls

	numbers = map read . words . reverse

	split [] = []
	split (l:ls) = (l:x) : split ls'
		where (x, ls') = span (not . (flip elem "+*")) ls


main = mapM_ print . sequence [day6_2] =<< readFile "day6_input"
