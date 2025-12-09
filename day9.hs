

day9 _ = ""

day9_2 _ = ""


main = mapM_ print . sequence [day9, day9_2] =<< readFile "day9_input"

