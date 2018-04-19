module Main where

import System.Environment

import Books

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn  "No command provided."
    (cmd:args') -> case lookup cmd dispatch of
      Just action -> action args'
      Nothing -> putStrLn $ "Action \"" ++ cmd ++ "\" not recognized."

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("list", list)
           , ("add", add)
           , ("remove", remove)
           , ("rm", remove)
           , ("help", help)
           ]

list :: [String] -> IO ()
list = undefined

add :: [String] -> IO ()
add = undefined

remove :: [String] -> IO ()
remove = undefined

help :: [String] -> IO ()
help _ = putStrLn "Manage your books. Commands are \"list\", \"add\", \"remove\" or \"rm\", \"help\"."
