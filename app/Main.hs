module Main where

import System.Environment

import Books

main :: IO ()
main = do
  (cmd:args) <- getArgs
  let (Just action) = lookup cmd dispatch
  action args

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
