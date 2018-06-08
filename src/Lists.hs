module Lists
  ( at
  , breakOn
  ) where

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at xs index = if index >= length xs || index < 0
                 then Nothing
                 else Just (xs !! index)

breakOn :: (a -> Bool) -> [a] -> [[a]]
breakOn p xs = case dropWhile p xs of
                 [] -> []
                 xs' -> x : breakOn p xs''
                   where (x, xs'') = break p xs'
