

day11 _ = ""

day11_2 _ = ""


main = mapM_ print . sequence [day11, day11_2] =<< readFile "day11_input"

