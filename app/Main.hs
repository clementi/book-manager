module Main where

import Control.Monad
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
           , ("rm", remove)
           , ("help", help)
           ]

list :: [String] -> IO ()
list _ = withFile fileName ReadMode (\h -> do
  contents <- hGetContents h
  let books = B.list contents
  forM_ books putStrBook)

putStrBook :: B.Book -> IO ()
putStrBook b = putStrLn $ (B.title b) ++ "\t" ++ (B.author b) ++ "\t" ++ (B.isbn b) ++ "\t" ++ (show $ B.pages b)

add :: [String] -> IO ()
add = undefined

remove :: [String] -> IO ()
remove = undefined

help :: [String] -> IO ()
help _ = putStrLn "Manage your books. Commands are \"list\", \"add\", \"remove\" or \"rm\", \"help\"."
