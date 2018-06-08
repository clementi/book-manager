module Lists
  ( at
  , segmentOn
  ) where

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at xs index = if index >= length xs || index < 0
                 then Nothing
                 else Just (xs !! index)

segmentOn :: (a -> Bool) -> [a] -> [[a]]
segmentOn p xs = case dropWhile p xs of
                 [] -> []
                 xs' -> x : segmentOn p xs''
                   where (x, xs'') = break p xs'
