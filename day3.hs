import Math.Combinat.Sets


day3 = sum . map (maximum . map read . choose 2) . lines

day3_2 = sum . map (maximum . map read . choose 12) . lines


main = readFile "day3_input" >>= mapM_ print . sequence [day3, day3_2]

