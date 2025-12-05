import Data.List.Split


main :: IO()
main = do
	c <- readFile "day4_input"
	print . day4 $ c
	print . day4_2 $ c


day4 = splitOn "\n"

day4_2 x = ""

