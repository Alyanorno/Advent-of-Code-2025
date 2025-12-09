

day10 _ = ""

day10_2 _ = ""


main = mapM_ print . sequence [day10, day10_2] =<< readFile "day10_input"

