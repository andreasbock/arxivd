module Utils.Levenshtein where

import Data.Array
import Data.Function
import Data.List

-- Taken from: https://wiki.haskell.org/Edit_distance
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = table ! (m, n)
    where (m, n) = (length xs, length ys)
          x      = array (1, m) (zip [1..] xs)
          y      = array (1, n) (zip [1..] ys)

          dist (0, j) = j
          dist (i, 0) = i
          dist (i, j) = minimum [table ! (i-1, j) + 1, table ! (i, j-1) + 1,
              if x ! i == y ! j then table ! (i-1, j-1) else 1 + table ! (i-1, j-1)]

          bnds  = ((0, 0), (m, n))
          table :: Array (Int, Int) Int
          table = array bnds [(ij, dist ij) | ij <- range bnds]

-- |Returns the top N best matches of a list of strings for a given string and returns
-- |a sorted (best matches first) list of pairs (s, i) where s is the string and i is
-- |the Levenshtein distance to the input string.
topMatchesN :: Eq a => [[a]] -> [a] -> Int -> [([a], Int)]
topMatchesN _  _ 0 = []
topMatchesN candidates s n = take n $ sortBy (compare `on` snd) pairs
    where mapped = map (editDistance s) candidates
          pairs  = zip candidates mapped

main :: IO ()
main = do
    print (editDistance "kitten" "sitting") -- Should be 3
    print $ topMatchesN ["kitten", "sitting", "coolios", "cup"] "sup" 2
