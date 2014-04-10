{- data-list.hs : Data.List module learning
 - rewrite and use functions found in Data.List
 - way to keep a record of what I know
 -}

{- Doesn't have any functions that clash with Prelude
 - so no need to qualify
 -}
import Data.List 
import Data.Function (on)
{- nub : returns a list with the duplicates removed -}

-- Returns the number of unique elements in a list
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

{- intersperse : takes in a element and puts it 
 - between every element in list 
 -}

--Returns the String as comma-seperated
commaSeperate :: String -> String
commaSeperate = intersperse $ ','

{- intercalate : puts the list in between lists 
 - in a list of lists, returns flattened list
 -}

-- Returns the words seperated by space
makeSentence :: [String] -> String
makeSentence = intercalate $ " "

{- transpose : matrix traspose a list of lists -}

-- Returns the transpose of a list of lists, interspersed with the
-- dummy rows given
transposeSpersed :: [a] -> [[a]] -> [[a]]
transposeSpersed dummy matrix = transpose $ intersperse dummy matrix

-- Sums up a bunch of polynomials , given their coeficents as inputs
sumPolynomial :: (Num a) => [[a]] -> [a]
sumPolynomial polyList = map sum $ transpose polyList

{- concat : flattens a list of lists into a list -}

-- flattens a list twice
flattenTwice :: [[[a]]] -> [a]
flattenTwice list = concat $ concat list

-- flattens list twice and sorts elements, removing duplicates
sortedUnique :: (Ord a) => [[[a]]] -> [a]
sortedUnique list = sort $ nub $ flattenTwice list

{- concatMap : maps function over list, then concats the list -}

-- replicates each element in the list the specified number of times
repeatList :: Int -> [a] -> [a] 
repeatList n list = concatMap (replicate n) list

{- and/or : and/or the entire list together (must be Bool) -}

--Returns if everything in the list fits some predicate
allTrue :: (a -> Bool) -> [a]  -> Bool
allTrue p list = and $ map p list

--Returns if at least one thing in the list fits some predicate
oneTrue :: (a -> Bool) -> [a] -> Bool
oneTrue p list = or $ map p list

{- any/all : see if any/all items in list make the predicate 
 - evaluate to true 
 -}

--Returns if input is all uppercase
isCapsLocked :: String -> Bool
isCapsLocked text = all (`elem` ['A'..'Z']) text

--Returns if input contains more than one word
isPhrase :: String -> Bool
isPhrase text = any (==' ') text

{- iterate : keeps reapplying function to value, keeping list of results -}

-- Grab the first n results of repeatedly applying the function to a list
firstNAns :: Int -> (a -> a) -> a -> [a]
firstNAns n f b = take n $ iterate f b

{- splitAt : split list at the index
 - nothing to really make out of it thats quick
 - just writing here for documentation purposes
 -}

{- take/dropWhile : takes/drop from list while predicate holds, 
 - stops/takes once predicate fails once
 -}

-- Takes the sum of all nth powers under m, n and m specified by user
sumNthPowers :: (Num a, Ord a, Integral a, Enum a) => a -> a -> a
sumNthPowers n m = sum $ takeWhile (<m) $ map (^n) [1..]

-- Trim off whitespace surrounding string
stringTrim :: String -> String
stringTrim text =  rev $ dropSpace $ rev $ dropSpace text
  where dropSpace = \x -> dropWhile (==' ') x
        rev = reverse

{- span/break : returns pair of lists broken at the point where 
 - the predicate first returned false/true.
 -}

{- sort : sorts a list 
 - group : goups adjacent elements together into sublists if equal
 -}

-- Outputs (a, count) for all a in [a]
elemCount :: (Ord a, Eq a) => [a] -> [(a,Int)]
elemCount list = map (\l@(x:_) -> (x,length l)) . group . sort $ list

{- inits/tails : similar to init and tail, but recursively applied on 
 - the list till nothing is left
 -}

-- Search a list for a contiguous subsequence
searchSubList :: (Eq a) => [a] -> [a] -> Bool
searchSubList needle haystack = 
  let nlen = length needle
  in 
  foldl 
  (\acc x -> if take nlen x == needle then True else acc) 
  False 
  (tails haystack)

{- isInfixOf - searchs for a sublist within a list 
 - isPrefixOf - searches for a sublist at the beginning of the list
 - isSuffixOf - searches for a sublist as the end of a list
 -}

{- elem/notElem : check if something is/is not in the list -}

{- partition : partitions the list into two lists, first with all 
 - elements that satisfied the predicate, and the second with all that
 - did not
 -}

-- My implementation of partition using filter
partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' p list = (yes,no)
  where yes = filter (\x -> p x) list
        no = filter (\x -> not $ p x) list

-- Better version, requiring only one pass through the list
partition'' :: (a -> Bool) -> [a] -> ([a],[a])
partition'' p list = 
  (\(x,y) -> (reverse x, reverse y)) $ partitionHelper p list ([],[])
  where partitionHelper p list (y,n) = 
          case list of [] -> (y,n)
                       (x:xs) -> if (p x) 
                                 then partitionHelper p xs (x:y,n) 
                                 else partitionHelper p xs (y,x:n)

{- find : searches a [a] for the first item that satisfies the predicate.
 - returns a Maybe a
 -}

-- Print out a string when you find some stock price thing
underLimit :: (Num a,Ord a) => a -> [(a,b,c,d)] -> Maybe (a,b,c,d)
underLimit n stockPrices = find (\(val,y,m,d) -> val > n) stockPrices

{- elemIndex : returns either Nothing or Just a, where a is the index of
 - the element you searched for
 -}

{- findIndex/findIndicies : returns the index of the first element that
 - satisfies the predicate / a list of the indicies of the satisfying 
 - elements
 -}

{- zipN/zipWithN : zip together N lists, with or without a combine 
 - function 
 -}

{- STRING MANIPULATION ONLY
 -
 - lines : takes in a string and returns a list of strings, which is
 - the original string split by thr '\n' character
 -
 - unlines : takes in list of strings, returns a single string that
 - is the list strings concatenated with '\n' as a delimiter
 -
 - words : split line of text based on spaces, handles the 
 - multiple adjacent spaces case
 -
 - unwords : joins a list of strings into a single string equaling the
 - list strings concatenated and delimited by a space
 -}

-- Grabs the 2nd line of the output and prints out the string, one word
-- per line
secondLine :: String -> String
secondLine text = let list = lines text
                  in if (length list < 2) 
                     then ""
                     else unlines $ words $ list !! 1

{- delete : deletes the first occurence of the element in the list -}

{- PSUEDO-SET FUNCTIONS
 -
 - \\ : list difference
 - 
 - union : union of first and second lists, duplicates removed from the
 - second one 
 - 
 - intersect : list intersection
 -}

-- returns the symmetric difference (xor) of a list of lists
symDifference :: (Ord a) => [[a]] -> [a]
symDifference lists = foldl (\a b -> a `union` b) [] lists

{- insert : insert the element into the list, putting it right before the
 - first element that is of equal or greater order
 -}


{- ALREADY LEARNED FUNCTIONS
 -
 - length : gets length of list
 - 
 - take : grab first n elements
 - 
 - drop : drop first n elements
 - 
 - splitAt : split list at index
 - 
 - !! : index into list, 0-indexed
 - 
 - replicate : replicate list
 -
 - Each of these has a general version, "generic_", which supports more 
 - than just Int as the type of the number paramater/return value
 -}

{- GENERAL EQUALITY
 -
 - nub, delete, union, intersect, and group have more general functions,
 - "_By", that take in an equality comparison to do the comparisonsi
 -} 

-- Takes in the delta stock price by day over the year, groups it 
-- into contiguous runs of positive and negative growth
grabRuns :: (Num a, Ord a) => [(a,a,a)] -> [[(a,a,a)]]
grabRuns stocks = 
  groupBy 
  (\(delta1,d1,m1) (delta2,d2,m2) -> (delta1 > 0) == (delta2 > 0)) 
  stocks

{- on : allows you to apply a function to both inpts, then apply
 - a second function to the results of the two applications.
 -
 - type specification:
 - 
 - on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
 - f `on` g = \x y -> f (g x) (g y)
 -}

-- parse CSV file given as csv lines with \n to delimit them
parseCSV :: String -> [String]
parseCSV document = 
  let xs = map (groupBy ((==) `on` (==','))) $ lines document
  in map (unwords . filter (/=",")) xs

{- ORDERING-RELATED FUNCTIONS
 - 
 - sort, insert, maximum, and minimum have more general equivalents, 
 - "_By", that also take in a custom ordering
 -}

-- Sort a list of lists
sortListList :: (Ord a) => [[a]] -> [[a]]
sortListList = sortBy (compare `on` length)
