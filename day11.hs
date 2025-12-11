import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map


day11 = traverse "you" . parse
	where
	traverse "out" _ = 1
	traverse node graph = sum $ map (`traverse` graph) $ fromJust $ Map.lookup node graph

day11_2 input = Map.findWithDefault 0 ("svr", 0) paths
	where
	graph = parse input
	paths = Map.fromList [((n, s), traverse n s) | n <- "out" : Map.keys graph, s <- [0..3]]

	traverse "out" x = x `div` 3
	traverse node s = sum [Map.findWithDefault 0 (c, next s node) paths | c <- Map.findWithDefault [] node graph]

	next 0 "dac" = 1
	next 0 "fft" = 2
	next 1 "fft" = 3
	next 2 "dac" = 3
	next s _ = s

parse = Map.fromList . map ((\(key:ls) -> (init key, ls)) . words) . lines

main = mapM_ print . sequence [day11, day11_2] =<< readFile "day11_input"

