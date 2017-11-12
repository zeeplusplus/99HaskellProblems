-- Problem 1: find the last element of a list
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = head (reverse xs)
myLast []     = error "myLast: empty list"

-- Problem 2: find the last but one element of a list
myButLast :: [a] -> a
myButLast []       = error "myLast: too few elements"
myButLast [x]      = error "myLast: too few elements"
myButLast [x, _]   = x
myButLast (_:xs)   = myButLast xs