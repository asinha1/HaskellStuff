{- sqrt-sums.hs : solves the squareroot-sums problem
 - 
 - Given input n, the squareroot-sums problems is defined as: How many elements
 - does it take for the sum of the roots of all natural numbers to exceede n?
 -}

sqrtSums:: (Floating a, Ord a, Enum a, Integral b) => a -> b
sqrtSums n =
  let ans = length (takeWhile (<n) (scanl1 (+) (map sqrt [1..]))) + 1
  in fromIntegral ans


