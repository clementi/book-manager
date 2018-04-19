module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Books as B

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
list _ = B.list

add :: [String] -> IO ()
add = undefined

remove :: [String] -> IO ()
remove = undefined

help :: [String] -> IO ()
help _ = putStrLn "Manage your books. Commands are \"list\", \"add\", \"remove\" or \"rm\", \"help\"."
