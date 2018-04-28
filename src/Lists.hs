module Lists
  ( at
  ) where

at :: [a] -> Int -> Maybe a
at [] index = Nothing
at xs index = if index >= length xs || index < 0
                 then Nothing
                 else Just (xs !! index)


