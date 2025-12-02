import Data.List.Split


main :: IO()
main = do
	c <- readFile "day2_input"
	print . day2 $ c
	print . day2_2 $ c


day2 = sum . concat . map (foo . map read . splitOn "-") . splitOn ","
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


day2_2 = concat . map (foo . map read . splitOn "-") . splitOn ","
	where
	foo :: [Int] -> [Int]
	foo (x:y:[]) = baa x y
		where
		baa :: Int -> Int -> [Int]
		baa start end
			| start == end = []
			| equal [chunksOf s a | s <- divisors (length a)] = read a : baa (start+1) end
			| otherwise = baa (start+1) end
			where
				equal (l1:l2:ls)
					| l1 == l2 = equal (l2:ls)
					| otherwise = False
				equal _ = True

				a = show start

				divisors :: Int -> [Int]
				divisors n = [x | x <- [1.. (n `div` 2)], n `rem` x == 0]

