import System.IO
import Data.List (findIndex)
import Data.Maybe (fromJust)


day3 = solve 2 . lines

day3_2 = solve 12 . lines

solve x = sum . map (read . max)
	where max s = step x 0
		where
		step 0 _ = ""
		step x start = top : step (x - 1) (start + (fromJust $ findIndex (== top) window) + 1)
			where
			window = take (length s - x - start + 1) (drop start s)
			top = maximum window

main = mapM_ print . sequence [day3, day3_2] =<< readFile "day3_input"

