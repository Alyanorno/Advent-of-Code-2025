

day1 = length . filter (== 0) . map (`mod` 100) . scanl (+) 50 . map parse . words

day1_2 = fst . last . scanl step (0, 50) . concatMap expand . map parse . words
	where
	expand n = replicate (abs n) (signum n)
	step (c, p) d = (c + fromEnum (p == 0), (p + d) `mod` 100)

parse ('L':xs) = 0 - read xs
parse ('R':xs) = read xs


main = readFile "day1_input" >>= mapM_ print . sequence [day1, day1_2]
