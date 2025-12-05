import Data.List.Split
import Data.List


main :: IO()
main = do
	c <- readFile "day3_input"
	print . day3 $ c
	print . day3_2 $ c


day3 = sum . map (take 1 . reverse . sort . read . \x -> [a ++ b | a <- x, b <- x] ) . words


day3_2 x = ""

