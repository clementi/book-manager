module Main where

import Control.Monad
import Data.List (delete)
import System.Environment
import System.Exit
import System.IO

import qualified Books as B

fileName :: FilePath
fileName = "books.tsv"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> (hPutStrLn stderr "No command provided.") >> exitFailure
    (cmd:args') -> runCommand cmd args'

runCommand :: String -> [String] -> IO ()
runCommand cmd args = case lookup cmd dispatch of
                        Just action -> action args
                        Nothing -> (hPutStrLn stderr $ "Action \"" ++ cmd ++ "\" not recognized.") >> exitFailure

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("list", list)
           , ("add", add)
           , ("remove", remove)
           , ("help", help)
           ]

putBooksLn :: [B.Book] -> IO ()
putBooksLn books = forM_ (zip [1..] books) (\(n, b) -> putStrLn $ (show n) ++ " " ++ B.title b)

list :: [String] -> IO ()
list _ = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  let books = B.list contents
  putBooksLn books)

add :: [String] -> IO ()
add (title:isbn:author:pages:_) = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  let books = B.list contents
      newBook = B.Book title isbn author (read pages :: Int)
      allBooks = newBook:books
  putBooksLn allBooks)

remove :: [String] -> IO ()
remove (bookId:_) = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  let books = B.list contents
      book = books !! ((read bookId :: Int) - 1)
      remainingBooks = delete book books
  putBooksLn remainingBooks)

help :: [String] -> IO ()
help _ = putStrLn "Manage your books. Commands are \"list\", \"add\", \"remove\" or \"rm\", \"help\"."
