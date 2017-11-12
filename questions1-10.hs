-- Problem 1: Find the last element of a list
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = head (reverse xs)
myLast []     = error "myLast: empty list"

-- Problem 2: Find the last but one element of a list
myButLast :: [a] -> a
myButLast []       = error "myButLast: too few elements"
myButLast [x]      = error "myButLast: too few elements"
myButLast [x, _]   = x
myButLast (_:xs)   = myButLast xs

-- Problem 3: Find the K'th element of a list. The first element in the list is number 1
elementAt :: (Integral b) => [a] -> b -> a
elementAt [] _ = error "elementAt: invalid input"
elementAt (x:xs) n
    | length xs < (fromIntegral(n) - 1) = error "elementAt: invalid input"
    | n == 1    = x
    | otherwise = elementAt xs (n - 1)