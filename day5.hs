

day5 _ = ""

day5_2 _ = ""


main = mapM_ print . sequence [day5, day5_2] =<< readFile "day5_input"

