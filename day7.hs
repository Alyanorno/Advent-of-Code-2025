

day7 _ = ""

day7_2 _ = ""


main = mapM_ print . sequence [day7, day7_2] =<< readFile "day7_input"

