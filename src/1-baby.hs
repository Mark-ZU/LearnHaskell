doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

increasing :: (Ord a) => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:ys) = x <= y && increasing (y:ys)

-- patterns
-- pattern = result
-- ...
increasing' :: (Ord a) => [a] -> Bool
increasing' (x:y:ys) = x <= y && increasing(y:ys)
increasing' _ = True

-- pattern guards
-- | condition = result
-- ...
-- | otherwise = result
noVowels :: [Char] -> [Char]
noVowels [] = []
noVowels (x:xs)
  | x `elem` "aeiouAEIOU" = noVowels xs
  | otherwise = x : noVowels xs

-- case expression of pattern -> result
watch :: Int -> [Char]
watch n = show n ++ " o'clock and " ++ case n of
  0 -> "no moon"
  1 -> "a crescent moon"
  2 -> "a half moon"
  3 -> "a gibbous moon"
  4 -> "a full moon"
  5 -> "a gibbous moon"
  6 -> "a half moon"
  7 -> "a crescent moon"
  8 -> "no moon"
  _ -> "a UFO"

-- result where pattern = result
watch' :: Int -> [Char]
watch' n = show n ++ " o'clock and " ++ moon n
  where moon 0 = "no moon"
        moon 1 = "a crescent moon"
        moon 2 = "a half moon"
        moon 3 = "a gibbous moon"
        moon 4 = "a full moon"
        moon 5 = "a gibbous moon"
        moon 6 = "a half moon"
        moon 7 = "a crescent moon"
        moon 8 = "no moon"
        moon _ = "a UFO"

-- let pattern = result
-- ...
-- in expression
gravity :: (Fractional a) => a -> a
gravity r = let g = 6.67e-11
                m = 5.97e24
            in g * m / (r ^ 2)


