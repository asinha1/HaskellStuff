{- 
 - baby.hs : my first haskell file
 - contains things i am learning from learnyouahaskell.com
 -}

{- Basic stuff -}
doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                      then x
                      else x*2

{- Using list comprehensions -}
boomBangs xs  = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

hello xxs = [ [x | x <- xs, even x] | xs <- xxs]

addThree x y z = x + y + z

{- Basic function pattern matching -}
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe x = "Not between one and two"

-- factorial function
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * (factorial (n-1))

-- add two vectrs from R^2
addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- grab first and third element from list
headFirstThird :: [a] -> (a,a)
headFirstThird (x:_:y:_) = (x,y)
headFirstThird _ = error "Less than three elements! Can't run headFirstThird"

-- simple function to describe an input list
tell :: (Show a) => [a] -> String
tell [] = "Empty List"
tell (x:[]) = "One element list. Element: " ++ show x
tell (x:y:[]) = "Two element list. Elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = 
  "3+ element list. First two elements: " ++ show x ++ " and " ++ show y

-- basic "toString" function for a list
printAllElements :: (Show a) => [a] -> String
printAllElements [] = ""
printAllElements (x:[]) = show x
printAllElements (x:xs) = show x ++ " " ++ (printAllElements xs)

{- Different BMI calculators using guards -}
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Fat"
  | otherwise   = "Whale"

-- also uses where clauses
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | bmi <= skinny = "Under"
  | bmi <= normal = "Normal"
  | bmi <= fat    = "Fat"
  | otherwise     = "Whale"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
 
initials' :: String -> String -> String
initials' (f:_) (l:_) = f:"." ++ l:"."

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
  where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w,h) <- xs,let bmi = w / h ^ 2]

{- Basic function with case statements -}

describeList :: [a] -> String
describeList xs = 
  "The list is " ++ case xs of [] -> "empty."
                               [x] -> "a single thing."
                               xs -> "a long list."

{- Recursion -}
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "list is empty!"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x:(replicate' (n-1) x)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : (take' (n-1) xs)

addThree' :: Int -> Int -> Int -> Int
addThree' x y z = x + y + z

factorial' :: Integer -> Integer
factorial' n = product [1..n]

-- A more complicated list parser I wrote
parse' :: (Ord a,Num a) => [a] -> String
parse' [] = "empty list"
parse' [x]
  | x < 0  = "One negative number"
  | x == 0 = "One zero"
  | x > 0  = "One positive number"
parse' (x:xs)
  | length xs < minLen  = "short list"
  | length xs == minLen = "medium size list"
  | length xs > minLen  = "long list"
  where minLen = 3

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:(repeat' x)

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip (x:xs) (y:ys) = (x,y):(zip' xs ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = a `elem'` xs

{- Function Staging -}
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

{- Three versions of flip
 - flip' : uses a where to define the function we return
 - flip'' : uses the fact that there is only one input to create a function
 -          that will take in two more
 - flip''' : uses a lambda to make it obvious that its a new function being
 -           returned
 -}
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

flip''' :: (a->b->c) -> b -> a -> c
flip''' f = \x y -> f y x

--map and filter functions
map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a-> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- Returns the largest number under 100,000 divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0


