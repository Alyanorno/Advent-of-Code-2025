
main :: IO()
main = do
	c <- readFile "day1_input"
	print . day1 $ c
	print . day1_2 $ c


day1 = length . filter (==0) . map wrap . scanl (+) 50 . map parse . words
	where
	parse :: String -> Integer
	parse ('L':xs) = 0 - read xs
	parse (x:xs) = read xs

	wrap x
		| x>99 = wrap (x-100)
		| x<0 = wrap (x+100)
		| otherwise = x


day1_2 = f . last . scanl foo (0, 50) . map parse . words
	where
	f (x, _) = x

	parse :: String -> Integer
	parse ('L':xs) = 0 - read xs
	parse (x:xs) = read xs

	foo (i, x) y
		| y<0 = foo (i+z, w(x-1)) (y+1)
		| y==0 = (i, x)
		| y>0 = foo (i+z, w(x+1)) (y-1)
		where
		z = if x == 0 then 1 else 0
		w x
			| x>99 = x-100
			| x<0 = x+100
			| otherwise = x

