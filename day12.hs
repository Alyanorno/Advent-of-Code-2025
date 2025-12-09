

day12 _ = ""

day12_2 _ = ""


main = mapM_ print . sequence [day12, day12_2] =<< readFile "day12_input"

