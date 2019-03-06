module Lists
  ( at
  , segmentOn
  ) where

import Data.Maybe (listToMaybe)

at :: [a] -> Int -> Maybe a
at xs index = listToMaybe . drop index $ xs

segmentOn :: (a -> Bool) -> [a] -> [[a]]
segmentOn p xs = case dropWhile p xs of
                 [] -> []
                 xs' -> x : segmentOn p xs''
                   where (x, xs'') = break p xs'
