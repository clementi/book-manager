module Lists
  ( at
  , segmentOn
  ) where

at :: [a] -> Int -> Maybe a
at xs index
  | index >= length xs || index < 0 = Nothing
  | otherwise = Just (xs !! index)

segmentOn :: (a -> Bool) -> [a] -> [[a]]
segmentOn p xs = case dropWhile p xs of
                 [] -> []
                 xs' -> x : segmentOn p xs''
                   where (x, xs'') = break p xs'
