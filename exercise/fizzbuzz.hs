module Exercise where

-- http://vipprog.net/wiki/exercise.html#t52e5a48

import Data.List

fizzbuzzN :: Int -> String
fizzbuzzN n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

fizzbuzz :: Int -> String
fizzbuzz n = unwords fizzbuzz_list
  where fizzbuzz_list = map fizzbuzzN [1..n]
