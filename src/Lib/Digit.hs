module Lib.Digit
  ( timeToDigitalLines,
    secondsToDigitLines,
    secondsToDigitArray,
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
    _ -> error $ "Invalid digit" ++ (show n)

timeToDigitalLines :: [Int] -> [String]
timeToDigitalLines [m1, m2, s1, s2] =
  horizontalFlatten
    [ numberToDigit m1,
      numberToDigit m2,
      colon,
      numberToDigit s1,
      numberToDigit s2
    ]
timeToDigitalLines _ = error "Invalid time"

-- flatten 2d array to 1d array horizontally
-- [["a", "b"], ["c", "d"], ["e", "f"]] -> ["ace", "bdf"]
horizontalFlatten :: [[String]] -> [String]
horizontalFlatten xs = map concat (transpose xs)

secondsToDigitArray :: Int -> [Int]
secondsToDigitArray n =
  let seconds = n `mod` 60
      minutes = n `div` 60
      m1 = minutes `div` 10
      m2 = minutes `mod` 10
      s1 = seconds `div` 10
      s2 = seconds `mod` 10
   in [m1, m2, s1, s2]

secondsToDigitLines :: Int -> [String]
secondsToDigitLines n = timeToDigitalLines (secondsToDigitArray n)
