import Data.List.Split


main :: IO()
main = do
	c <- readFile "day2_input"
	print . day1 $ c

day1 = sum . concat . map (foo . map read . splitOn "-") . splitOn ","
	where
	foo :: [Integer] -> [Integer]
	foo (x:y:[]) = baa x y
		where
		baa :: Integer -> Integer -> [Integer]
		baa x y 
			| x == y = []
			| un_even (length a) = baa (x+1) y
			| (take s a) == (drop s a) = read a : baa (x+1) y
			| otherwise = baa (x+1) y
			where
				s = (length a) `div` 2
				a = show x
				un_even n = n `mod` 2 /= 0

