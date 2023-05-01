-- The empty board
ttt_init = ["...","...","..."]

other :: Char -> Char
other 'X' = 'O'
other 'O' = 'X'
other _ = error "Invalid input in other function"

-- Returns the winner
winner :: [[Char]] -> Char
winner [[a,b,c],[d,e,f],[g,h,i]]
        | a == b && b == c && a /= '.' = a
        | d == e && e == f && f /= '.' = d
        | g == h && h == i && i /= '.' = g
        | a == d && d == g && g /= '.' = a
        | b == e && e == h && h /= '.' = b
        | c == f && f == i && i /= '.' = c
        | a == e && e == i && i /= '.' = a
        | c == e && e == g && g /= '.' = c
        | '.' `notElem` [a,b,c,d,e,f,g,h,i] = 'T' -- Tie
        | otherwise = '.' -- No winner yet
winner _ = error "Invalid board"

-- Replace the ith element of a list
replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

-- Replaces the character at position (x,y) with c
replace2D :: Int -> Int -> Char -> [[Char]] -> [[Char]]
replace2D x y c xs = replace y (replace x c (xs !! y)) xs

play :: [[Char]] -> Char -> (Int,Int) -> [[Char]]
play board player (x,y) = replace2D x y player board

-- Returns the value of board with player to play from viewpoint
value :: [[Char]] -> Char -> Char -> Int
value board viewpoint player
        | winner board == viewpoint = 1
        | winner board == other viewpoint = -1
        | winner board == 'T' = 0
        | viewpoint == player = maximum [value (replace2D x y player board) viewpoint (other player) | x <- [0..2], y <- [0..2], board !! y !! x == '.']
        | viewpoint /= player = minimum [value (replace2D x y player board) viewpoint (other player) | x <- [0..2], y <- [0..2], board !! y !! x == '.']

value _ _ _ = error "Invalid input in value function"

-- Returns the best move for player on board
bestMove :: [[Char]] -> Char -> (Int,Int)
bestMove board player = snd (maximum [(value (replace2D x y player board) player (other player), (x,y)) | x <- [0..2], y <- [0..2], board !! y !! x == '.'])

aiX :: [[Char]] -> [[Char]]
aiX board = play board 'X' (bestMove board 'X')

aiO :: [[Char]] -> [[Char]]
aiO board = play board 'O' (bestMove board 'O')