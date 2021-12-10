-- Lib: helper functions

module Lib
  ( splitOn,
    trimLeft,
    parseIntOrDefault,
  )
where

import Data.Char (isSpace)
import Data.Maybe
import Model (State (..), Task (..))
import Text.Read

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s =
  case dropWhile (== c) s of
    [] -> []
    s' -> w : splitOn c s''
      where
        (w, s'') = break (== c) s'

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseIntOrDefault :: Read a => String -> a -> a
parseIntOrDefault s d =
  fromMaybe d (readMaybe s)
