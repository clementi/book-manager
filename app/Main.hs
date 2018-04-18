module Main where

import System.Environment

import Books

main :: IO ()
main = do
  (cmd:args) <- getArgs
  case lookup cmd dispatch of
    Just action -> action args
    Nothing -> putStrLn $ "Action \"" ++ cmd ++ "\" not recognized."

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("list", list)
           , ("add", add)
           , ("remove", remove)
           , ("rm", remove)
           ]

list :: [String] -> IO ()
list = undefined

add :: [String] -> IO ()
add = undefined

remove :: [String] -> IO ()
remove = undefined
