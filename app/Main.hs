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
parse [] = parse ["-h"]
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
list = do
  books <- B.load
  putBooksLn books

remove :: Int -> IO ()
remove n = do
  books <- B.load
  case books `at` (n - 1) of
    Just book -> do
      let newBooks = filter (/=book) books
      B.save newBooks
      putBooksLn newBooks
    Nothing -> (hPutStrLn stderr $ "No book at " ++ show n) >> exitFailure

add :: [String] -> IO ()
add details = do
  books <- B.load
  let newBook = B.fromList details
      allBooks = newBook:books
  B.save allBooks
  putBooksLn allBooks

details :: Int -> IO ()
details n = do
  books <- B.load
  case books `at` (n - 1) of
    Just book -> mapM_ putStrLn $ B.toList book
    Nothing -> (hPutStrLn stderr $ "No book at " ++ show n) >> exitFailure

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " [-vh] [cmd ..]"

version :: IO ()
version = putStrLn "manage 0.1"

putBooksLn :: [B.Book] -> IO ()
putBooksLn books = forM_ (zip [1..] books) (putStrLn . uncurry bookLine)

bookLine :: (Show a) => a -> B.Book -> String
bookLine n (B.Book title _ author _) = (show n) ++ " " ++ title ++ " (" ++ author ++ ")"
