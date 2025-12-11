import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map


day11 = traverse "you" . Map.fromList . map ((\(key:ls) -> (init key, ls)) . words) . lines
	where
	traverse node graph
		| node == "out" = 1
		| otherwise = sum $ map (flip traverse graph) $ fromJust $ Map.lookup node graph

day11_2 _ = 0


main = mapM_ print . sequence [day11, day11_2] =<< readFile "day11_input"

