module Main where

import Control.Monad
import Data.List
import System.Environment
import System.Exit
import System.IO
import qualified System.IO.Strict as S

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

list :: IO ()
list = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  putBooksLn $ getBooks contents)

remove :: Int -> IO ()
remove n = do
  contents <- S.readFile fileName
  let books = getBooks contents
  case books `at` (n - 1) of
    Just book -> do
      let newBooks = filter (/=book) books
          newContents = concat $ map fileBookLine newBooks
      writeFile fileName newContents
      putBooksLn newBooks
    Nothing -> (hPutStrLn stderr $ "No book at " ++ show n) >> exitFailure

add :: [String] -> IO ()
add details = do
  contents <- S.readFile fileName
  let books = getBooks contents
      newBook = B.fromList details
      allBooks = newBook:books
  putBooksLn allBooks
  let newContents = concat $ map fileBookLine allBooks
  writeFile fileName newContents

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

fileBookLine :: B.Book -> String
fileBookLine book = (concat $ intersperse "\t" $ B.toList book) ++ "\n"
