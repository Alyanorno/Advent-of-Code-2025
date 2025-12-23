import Data.List
import Data.List.Split
import Data.Bits (xor, setBit)
import qualified Data.IntMap.Strict as IntMap


day10 = sum . map minButtons . map (parse . words) . lines
	where
	parse ls = (parseTarget target, map parseButton $ init rest)
		where
		(target:[], rest) = splitAt 1 ls
		parseTarget = foldl' setBit 0 . map fst . filter ((==) '#' . snd) . zip [0..] . drop 1
		parseButton = foldl' setBit 0 . map read . splitOn "," . init . drop 1

	minButtons :: (Int, [Int]) -> Int
	minButtons (target, buttons) = minimum [length subset | subset <- subsequences buttons, (foldl' xor 0 subset) == target]

day10_2 :: String -> Int
day10_2 = sum . map minPresses . map parseMachine . lines
  where
    parseMachine line = (target, buttonMasks, buttonVectors)
      where
        parts = words line
        target = parseTarget $ last parts
        buttonParts = filter (\s -> s /= "" && head s == '(') $ init parts
        buttonMasks = map parseButtonMask buttonParts
        buttonVectors = map (parseButtonVector $ length target) buttonParts
        
        parseTarget s = map read $ splitOn "," $ init $ drop 1 s
        parseButtonMask s = foldl' setBit 0 $ parseIndices s
        parseButtonVector n s = [if i `elem` indices then 1 else 0 | i <- [0..n-1]]
          where indices = parseIndices s
        parseIndices "()" = []
        parseIndices s = map read $ splitOn "," $ init $ drop 1 s


minPresses :: ([Int], [Int], [[Int]]) -> Int
minPresses (target, buttonMasks, buttonVectors) = result
  where
    infinity = 10000000

    (result, _) = go target IntMap.empty

    n = length target
    buttonData = zip buttonMasks buttonVectors
    bases = scanl (*) 1 (map (+1) target)

    encodeState state = sum $ zipWith (*) state bases

    patternMap = IntMap.fromListWith (++) $ do
      subset <- subsequences buttonData
      let mask = foldl' xor 0 (map fst subset)
          effect = foldl' (zipWith (+)) (replicate n 0) (map snd subset)
      return (mask, [(effect, length subset)])

    go :: [Int] -> IntMap.IntMap Int -> (Int, IntMap.IntMap Int)
    go state memo
      | all (==0) state = (0, memo)
      | otherwise = case IntMap.lookup (encodeState state) memo of
          Just res -> (res, memo)
          Nothing -> 
            let parity = foldl' setBit 0 [i | (i, v) <- zip [0..] state, odd v]
                candidates = IntMap.findWithDefault [] parity patternMap
                
                step (best, m) (effect, size) =
                  let next = zipWith (-) state effect
                  in if all (>=0) next && all even next
                     then let (sub, m') = go (map (`div` 2) next) m
                          in if sub >= infinity
                             then (best, m')
                             else (min best (size + 2 * sub), m')
                     else (best, m)
                
                (best, memo') = foldl' step (infinity, memo) candidates
            in (best, IntMap.insert (encodeState state) best memo')


main = mapM_ print . sequence [day10, day10_2] =<< readFile "day10_input"

