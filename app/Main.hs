module Main where

import Control.Monad
import Data.List
import System.Environment
import System.Exit
import System.IO

import Strings
import Lists

import qualified Books as B

fileName :: FilePath
fileName = "books.tsv"

main :: IO ()
main = getArgs >>= parse >>= manage

parse :: [String] -> IO [String]
parse ["-h"] = usage >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse args = return args

manage :: [String] -> IO ()
manage ("list":_) = list
manage ("ls":_) = manage ["list"]
manage ("remove":n:_) = remove (read n :: Int)
manage ("rm":n:_) = manage ["remove", n]
manage ("add":details) = add details
manage ("details":n:_) = details (read n :: Int)
manage ("det":n:_) = manage ["details", n]

{-manage' :: [String] -> IO [B.Book]-}
{-manage' ("ls":_) = list'-}

{-loadBooks :: IO [B.Book]-}
{-loadBooks = withFile fileName ReadMode (\h -> do-}
  {-contents <- hGetContents h-}
  {-return $ getBooks contents)-}

{-list' :: IO [B.Book]-}
{-list' = loadBooks-}

list :: IO ()
list = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  putBooksLn $ getBooks contents)

remove :: Int -> IO ()
remove n = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  putBooksLn $ getBooks contents)

add :: [String] -> IO ()
add details = withFile fileName ReadWriteMode (\h -> do
  contents <- hGetContents h
  let books = getBooks contents
      newBook = B.fromList details
      allBooks = newBook:books
   in putStrLn $ intercalate "\n" $ map ((intercalate "\t") . B.toList) allBooks)

details :: Int -> IO ()
details n = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  case (getBooks contents) `at` (n - 1) of
    Just book -> mapM_ putStrLn $ B.toList book
    Nothing -> (hPutStrLn stderr $ "No book at " ++ show n) >> exitFailure)

getBooks :: String -> [B.Book]
getBooks contents = map parseBook $ lines contents

parseBook :: String -> B.Book
parseBook line = B.fromList $ wordsWhen (=='\t') line

usage :: IO ()
usage = putStrLn "Usage: manage: [-vh] [cmd ..]"

version :: IO ()
version = putStrLn "manage 0.1"

putBooksLn :: [B.Book] -> IO ()
putBooksLn books = forM_ (zip [1..] books) (putStrLn . uncurry bookLine)

bookLine :: (Show a) => a -> B.Book -> String
bookLine n (B.Book title _ author _) = (show n) ++ " " ++ title ++ " (" ++ author ++ ")"


