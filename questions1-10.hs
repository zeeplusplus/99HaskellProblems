import Debug.Trace
import Data.List

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

-- Problem 4: Find the number of elements of a list
myLength :: (Integral b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5: Reverse a list
myReverse :: [a] -> [a]
myReverse = foldl (\a x -> x:a) []

-- Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = take ((length xs) `div` 2) xs == reverse (drop ((length xs) `div` 2) xs)

-- Problem 7: Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- Problem 8: Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (y:xs)
    | otherwise = x : compress (y:xs)

-- Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
--             Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = countDup $ pack xs
    where countDup [] = []
          countDup (x:xs) = (length x, head x) : countDup xs