-- Lib: helper functions

module Lib
  ( splitOn,
    trimLeft,
  )
where

import Data.Char (isSpace)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s =
  case dropWhile (== c) s of
    [] -> []
    s' -> w : splitOn c s''
      where
        (w, s'') = break (== c) s'

trimLeft :: String -> String
trimLeft = dropWhile isSpace
