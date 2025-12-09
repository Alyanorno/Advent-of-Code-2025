

day8 _ = ""

day8_2 _ = ""


main = mapM_ print . sequence [day8, day8_2] =<< readFile "day8_input"

