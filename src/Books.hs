module Books
  ( Book (..)
  , list
  ) where

import System.IO

data Book = Book { title :: String
                 , isbn :: String
                 , author :: String
                 , pages :: Int
                 } deriving (Show)

fileName :: FilePath
fileName = "books.tsv"

list :: IO ()
list = withFile fileName ReadMode (\h -> do
                                      contents <- hGetContents h
                                      putStrLn contents)
