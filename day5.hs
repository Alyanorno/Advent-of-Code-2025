import Data.List.Split


main :: IO()
main = do
	c <- readFile "day5_input"
	print . day5 $ c
	print . day5_2 $ c


day5 = splitOn "\n"

day5_2 x = ""

