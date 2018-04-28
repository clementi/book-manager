module Books
  ( Book (..)
  , fromList
  , toList
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

toList :: Book -> [String]
toList (Book title isbn author pages) = [title, isbn, author, show pages]
