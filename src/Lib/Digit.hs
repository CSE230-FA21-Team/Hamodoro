module Lib.Digit
  ( timeToDigitalLines,
  )
where

import Data.Char (digitToInt)
import Data.List

colon :: [String]
colon =
  [ "..",
    "#.",
    "..",
    "#.",
    ".."
  ]

zero :: [String]
zero =
  [ "######.",
    "#....#.",
    "#....#.",
    "#....#.",
    "######."
  ]

one :: [String]
one =
  [ ".....#.",
    ".....#.",
    ".....#.",
    ".....#.",
    ".....#."
  ]

two :: [String]
two =
  [ "######.",
    ".....#.",
    "######.",
    "#......",
    "######."
  ]

three :: [String]
three =
  [ "######.",
    ".....#.",
    "...###.",
    ".....#.",
    "######."
  ]

four :: [String]
four =
  [ "#......",
    "#......",
    "#...#..",
    "######.",
    "....#.."
  ]

five :: [String]
five =
  [ "######.",
    "#......",
    "######.",
    ".....#.",
    "######."
  ]

six :: [String]
six =
  [ "######.",
    "#......",
    "######.",
    "#....#.",
    "######."
  ]

seven :: [String]
seven =
  [ "######.",
    ".....#.",
    ".....#.",
    ".....#.",
    ".....#."
  ]

eight :: [String]
eight =
  [ "######.",
    "#....#.",
    "######.",
    "#....#.",
    "######."
  ]

nine :: [String]
nine =
  [ "######.",
    "#....#.",
    "######.",
    ".....#.",
    "######."
  ]

numberToDigit :: Int -> [String]
numberToDigit n =
  case n of
    0 -> zero
    1 -> one
    2 -> two
    3 -> three
    4 -> four
    5 -> five
    6 -> six
    7 -> seven
    8 -> eight
    9 -> nine
    _ -> error "Invalid digit"

timeToDigitalLines :: [Int] -> [String]
timeToDigitalLines [h1, h2, m1, m2] =
  horizontalFlatten
    [ numberToDigit h1,
      numberToDigit h2,
      colon,
      numberToDigit m1,
      numberToDigit m2
    ]
timeToDigitalLines _ = error "Invalid time"

-- flatten 2d array to 1d array horizontally
-- [["a", "b"], ["c", "d"], ["e", "f"]] -> ["ace", "bdf"]
horizontalFlatten :: [[String]] -> [String]
horizontalFlatten xs = map concat (transpose xs)
