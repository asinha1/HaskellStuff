{- collatz.hs : for the Collatz sequence length problem:
 - 
 - Collatz algorithm: 
 -  if number is 1 return
 -  if number is even then collatz(n/2)
 -  if number is odd then collatz(n*3 +1)
 -
 - Let us define a chain as the sequence of numbers till the Collatz algorithm
 - terminates.
 - Then for all positive nonzero numbers n and m, the Collatz sequence length 
 - problem is as follows: for all starting numbers between 1 and n, how many 
 - chains have a length greater than m?
 -}

-- chain : genetates a list of the Collatz evaluation sequence for an input n
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n*3 + 1)

-- basic answer from tutorial to get the code working. Assumes n=100, m=15
numChainsLonger15 :: Int
numChainsLonger15 = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

-- robust numLongChains, which lets you input the range and length cutoff
numLongChains :: (Integral a) => a -> a -> a
numLongChains n m = 
  let ans = length (filter isLong (map chain [1..n]))
  in  fromIntegral(ans)
  where isLong xs = fromIntegral(length xs) > m

-- numLongChains written with lambda functions
numLongChains :: (Integral a) => a -> a -> a
numLongChains n m = 
  let ans = length (filter (\xs -> length xs > m) (map chain [1..n]))
  in  fromIntegral(ans)
