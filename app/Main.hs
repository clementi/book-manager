module Main where

import Control.Monad
import Data.List (intercalate)
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

list :: [String] -> IO ()
list _ = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  let books = B.list contents
  forM_ (zip [1..] books) (\(n, b) -> putStrLn $ (show n) ++ " " ++ B.title b))

add :: [String] -> IO ()
add = undefined

remove :: [String] -> IO ()
remove = undefined

help :: [String] -> IO ()
help _ = putStrLn "Manage your books. Commands are \"list\", \"add\", \"remove\" or \"rm\", \"help\"."
