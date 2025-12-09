

day6 _ = ""

day6_2 _ = ""


main = mapM_ print . sequence [day6, day6_2] =<< readFile "day6_input"

