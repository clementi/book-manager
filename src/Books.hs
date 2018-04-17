module Books
  ( Book (..)
  ) where

data Book = Book { title :: String
                 , isbn :: String
                 , author :: String
                 , pages :: Int
                 } deriving (Show)
