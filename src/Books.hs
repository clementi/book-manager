module Books
  ( Book (..)
  , fromList
  , toList
  , load
  , save
  ) where

import Data.List

import qualified System.IO.Strict as S

import Lists

data Book = Book { title :: String
                 , isbn :: String
                 , author :: String
                 , pages :: Int
                 } deriving (Eq, Show)

fileName :: FilePath
fileName = "books.tsv"

fromList :: [String] -> Book
fromList [title, isbn, author, pages] = Book title isbn author (read pages :: Int)

toList :: Book -> [String]
toList (Book title isbn author pages) = [title, isbn, author, show pages]

load :: IO [Book]
load = do
  contents <- S.readFile fileName
  return $ getBooks contents

save :: [Book] -> IO ()
save books = do
  let contents = concat $ map fileBookLine books
  writeFile fileName contents

getBooks :: String -> [Book]
getBooks = map parseBook . lines

parseBook :: String -> Book
parseBook = fromList . segmentOn (=='\t')

fileBookLine :: Book -> String
fileBookLine book = (concat $ intersperse "\t" $ toList book) ++ "\n"
