module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.IO

import qualified Books as B
import qualified Strings as S

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

loadBooks :: IO [B.Book]
loadBooks = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  return $ getBooks contents)

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
add details = putStrLn $ show $ B.fromList details

details :: Int -> IO ()
details n = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  let book = (getBooks contents) !! (n - 1)
   in mapM_ putStrLn $ B.toList book)

getBooks :: String -> [B.Book]
getBooks contents = map parseBook $ lines contents

parseBook :: String -> B.Book
parseBook line = B.fromList $ S.wordsWhen (=='\t') line

usage :: IO ()
usage = putStrLn "Usage: manage: [-vh] [cmd ..]"

version :: IO ()
version = putStrLn "manage 0.1"

putBooksLn :: [B.Book] -> IO ()
putBooksLn books = forM_ (zip [1..] books) (\(n, b) -> putStrLn $ (show n) ++ " " ++ B.title b)

