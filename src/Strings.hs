module Strings
  ( wordsWhen
  ) where

import Lists

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen = breakOn
