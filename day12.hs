import Data.List
import Data.Bits
import Data.Ord


day12 :: String -> Int
day12 = length . (\(shapes, regions) -> [() | (w, h, need) <- regions, check w h shapes need]) . parse . lines

parse ls = (shapes, regions)
	where
	(shapeLines, regLines) = break (\l -> 'x' `elem` l && ':' `elem` l) ls

	shapes = reverse $ build [] shapeLines
	build acc [] = acc
	build acc (l:ls)
		| ':' `elem` l && 'x' `notElem` l = build (sort [(x-minX, y-minY) | (x,y) <- coords] : acc) rest
		| otherwise = build acc ls
			where
			(block, rest) = break (\l -> ':' `elem` l && 'x' `notElem` l) ls
			coords = [(x,y) | (y,l) <- zip [0..] block, (x,c) <- zip [0..] l, c == '#']
			minX = minimum (map fst coords)
			minY = minimum (map snd coords)
	regions = [(read wS, read hS, map read $ words after) | l <- regLines, not (null l), let (wh, _:after) = break (==':') l, let (wS, 'x':hS) = break (== 'x') wh]

check w h shapes need = search need 0
	where
	area = w * h
	shapeSizes = map length shapes

	shapeMasks = [nub $ concatMap generate $ nub [normalize [f (x,y) | (x,y) <- shape] | f <- translate] | shape <- shapes]
		where
		translate = [id, \(x,y)->(y,-x), \(x,y)->(-x,-y), \(x,y)->(-y,x), \(x,y)->(-x,y), \(x,y)->(y,x), \(x,y)->(x,-y), \(x,y)->(-y,-x)]
		normalize ps = sort [(x-minX, y-minY) | (x,y) <- ps]
			where
			minX = minimum (map fst ps)
			minY = minimum (map snd ps)

	        generate coords = [foldl' setBit 0 [ (y+dy)*w + (x+dx) | (x,y) <- coords] | dx <- [0..w-maxX-1], dy <- [0..h-maxY-1]]
			where
			maxX = maximum (map fst coords)
			maxY = maximum (map snd coords)

	search counts occupied
		| all (==0) counts = True
		| cellsNeeded > free = False
		| otherwise = any tryShape topPlacements
		where
		cellsNeeded = sum [c * (shapeSizes!!i) | (i,c) <- zip [0..] counts, c > 0]
		free = area - popCount occupied
		options :: [(Int, [Int])] = [(i, filter (\m -> m .&. occupied == 0) (shapeMasks!!i)) | (i,c) <- zip [0..] counts, c > 0]
		(topId, topPlacements) = minimumBy (comparing length) options
		tryShape mask = search (take topId counts ++ (counts!!topId - 1) : drop (topId+1) counts) (occupied .|. mask)

day12_2 _ = 0


main = mapM_ print . sequence [day12, day12_2] =<< readFile "day12_input"

