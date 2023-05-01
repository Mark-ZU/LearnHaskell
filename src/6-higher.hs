map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' [x] [] = []
zip' [] [x] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs)
        | f x = x : takeWhile' f xs
        | otherwise = []

-- lambda functions
accList :: Num a => [a] -> [a]
accList a = map (\x -> x + 1) a

foldl' f start [] = start
foldl' f start (x:xs) = foldl' f (f start x) xs
