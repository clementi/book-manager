module Books
  ( Book (..)
  , fromList
  ) where

import System.IO
import Data.List.Split

data Book = Book { title :: String
                 , isbn :: String
                 , author :: String
                 , pages :: Int
                 } deriving (Eq, Show)

fromList :: [String] -> Book
fromList [title, isbn, author, pages] = Book title isbn author (read pages :: Int)

