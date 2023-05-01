import Data.List

uniqueNum :: Eq a => [a] -> Int
uniqueNum = length . nub

----------------------------------------
transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([]: xss) = transpose' xss
transpose' ((x:xs):xss) = (x: [h | (h:_) <- xss]) : transpose' (xs: [t | (_:t) <- xss])

-- library
-- transpose :: [[a]] -> [[a]]
-- transpose [] = []
-- transpose ([] : xss) = transpose xss
-- transpose ((x : xs) : xss) = combine x hds xs tls
--   where
--     (hds, tls) = unzip [(hd, tl) | hd : tl <- xss]
--     combine y h ys t = (y:h) : transpose (ys:t)

-- f $ g $ h x  =  f (g (h x))
testunzip xss = unzip [(h,d)| h:d <- xss]