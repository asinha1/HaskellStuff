{- fold-scan.hs : for all of the fold/scan-related things
 - foldl : left fold (starts from left, moves to right)
 - foldr : right fold
 - scanl : foldl but prints out every partial ans as well as the final
 - scanr : foldr with scanl's extra property
 -}

-- basic foldl stuff
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\prev x -> if x==y then True else prev) False ys

-- basic foldr stuff

-- use foldr because foldl would require our lambda to do "acc ++ [f x]",
-- which is super expensive
map' :: (a->b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

{- foldl1 and foldr1 : foldl/r without the base case
 - they assume the base case is the first element, and then start the fold
 - with the first and second element (if there is one)
 -}
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x curMax -> if x > curMax then x else curMax)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- basic scanl stuff
partialSums :: (Num a) => [a] -> [a]
partialSums xs = scanl (+) 0 xs

-- basic scanr stuff

{- scanl1 and scanr1 : scanl/r without the base case
 - they assume the base case is the first element, and then start the scan
 - with the first and second element (if there is one)
 -}

