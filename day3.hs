import Math.Combinat.Sets


day3 = solve 2

day3_2 = solve 12

solve n = sum . map (maximum . map read . choose n) . lines

main = mapM_ print . sequence [day3, day3_2] =<< readFile "day3_input"

