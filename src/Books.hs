module Books
  ( Book (..)
  , list
  ) where

import System.IO
import Data.List.Split

data Book = Book { title :: String
                 , isbn :: String
                 , author :: String
                 , pages :: Int
                 } deriving (Eq, Show)

list :: String -> [Book]
list contents = map loadBook $ lines contents

loadBook :: String -> Book
loadBook line = let parts = splitWhen (=='\t') line
                 in Book (parts !! 0) (parts !! 1) (parts !! 2) (read (parts !! 3) :: Int)

add :: Handle -> Book -> [Book] -> IO [Book]
add _ _ _ = return []

remove :: Handle -> Book -> [Book] -> IO [Book]
remove _ _ _ = return []
