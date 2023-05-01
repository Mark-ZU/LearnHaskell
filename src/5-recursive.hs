length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)