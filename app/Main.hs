module Main where

import Control.Monad
import Data.List
import Safe
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

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err maybeVal = case maybeVal of
  Just x -> Right x
  Nothing -> Left err

runCommand :: String -> [String] -> IO ()
runCommand cmd args = case findAction cmd >>= (\action -> action args) of
                        Left err -> (hPutStrLn stderr err) >> exitFailure
                        Right books -> putBooksLn books

findAction :: String -> Either String ([String] -> Either String [B.Book])
findAction cmd = maybeToEither ("Action \"" ++ cmd ++ "\" not recognized.") $ lookup cmd dispatch

dispatch :: [(String, [String] -> Either String [B.Book])]
dispatch = [ ("list", list)
           , ("add", add)
           , ("remove", remove)
           ]

list :: [String] -> Either String [B.Book]
list _ = Left "bad!"

add :: [String] -> Either String [B.Book]
add _ = Right []

remove :: [String] -> Either String [B.Book]
remove _ = Right []

putBooksLn :: [B.Book] -> IO ()
putBooksLn books = forM_ (zip [1..] books) (\(n, b) -> putStrLn $ (show n) ++ " " ++ B.title b)

-- list :: [String] -> IO ()
-- list _ = withFile fileName ReadMode (\h -> do
--   contents <- hGetContents h
--   let books = B.list contents
--   putBooksLn books)

-- add :: [String] -> IO ()
-- add (title:isbn:author:pages:_) = withFile fileName ReadMode (\h -> do
--   contents <- hGetContents h
--   let books = B.list contents
--       pageCount = read pages :: Int
--       newBook = B.Book title isbn author pageCount
--       allBooks = newBook:books
--   putBooksLn allBooks)

-- remove :: [String] -> IO ()
-- remove (bookId:_) = withFile fileName ReadMode (\h -> do
--   contents <- hGetContents h
--   let books = B.list contents
--       index = (read bookId :: Int) - 1
--   case books `atMay` index of
--     Just book -> putBooksLn $ delete book books
--     Nothing -> (putStrLn $ "No book at " ++ bookId) >> exitFailure)

