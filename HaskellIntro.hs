{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit x = if x < 10 then x else x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = if x < 10 then 0 else x `div` 10

toDigits :: Integer -> [Integer]
toDigits x = if x <= 0 then [] else toDigits (dropLastDigit x) ++ [lastDigit x] --append list versions of the digits to each other instead of cons since it lets us do things in a right to left order

doubleEveryOtherH :: [Integer] -> [Integer]
doubleEveryOtherH [] = [] --if list is empty just return it
doubleEveryOtherH [x] = [x] --if list is a single element there's nothing to check out
doubleEveryOtherH (x:x2:xs) = x : (2 * x2 : (doubleEveryOtherH xs)) --isolate the first 2 elements of the list, then double the second, and finally perform the same operation on the rest of the list

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherH (reverse xs)) --use helper to do operation on a reversed version of the list

sumDigsH :: [Integer] -> Integer --helper for sumDigits
sumDigsH xs = foldl (+) 0 xs

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 (map sumDigsH (map toDigits xs)) --hacky use of helper functions. Digiconverters all elements to become lists of single digit elements, uses map to add those up, then folds the list in to add those subtotals


--Assumes check digit is the final digit. Drops it in the calculations, but revisits it for the final comparison
--Double the value of every second digit from the right after dropping final digit. Uses doubleEveryOther
--sum the digits of this new list. Uses sumDigits.
--the check digit should equal to this sum mod 10
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x)) `mod` 10) == 0

--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow _ 0 = id 
pow f n = f . (pow f (n-1))

g :: Integer -> Integer
g 0 = 0
g n = n - ((pow g 2) (n-1))

h :: Integer -> Integer
h 0 = 0
h n = n - ((pow h 3) (n-1))

d :: Int -> Integer -> Integer
d _ 0 = 0
d i n = n - ((pow (d i) i) (n-1))

--
-- Problem 3
--

-- If we're given an empty set, just put it in a set. 
-- If we're not, then we have to do the algorithm described in lecture notes
-- Separate the first element from the set. We'll call this x.
-- Find every subset of T and insert x into it. 
powerSet :: Ord a => Set a -> Set(Set a)
powerSet set = if isEmpty set 
               then singleton set
               else singleton set --bad implementation so it compiles and can run the other tests

--Comment to explain my understanding of the concept, but lack of understanding in application
--I know I am meant to extract the first element of the input set, then apply it to the powerset of the remaining items.
--I know I must then take that application and make a union with an un-touched copy powerset of the remaining items
--However, I do not understand how to apply this into code as the exact syntax of set has been mystifying me. 
  
