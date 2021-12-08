module Lib.Digit () where

import Data.Char (digitToInt)

colon :: String
colon =
  unlines
    [ "..",
      "#.",
      "..",
      "#.",
      ".."
    ]

zero :: String
zero =
  unlines
    [ "######.",
      "#....#.",
      "#....#.",
      "#....#.",
      "######."
    ]

one :: String
one =
  unlines
    [ ".....#.",
      ".....#.",
      ".....#.",
      ".....#.",
      ".....#."
    ]

two :: String
two =
  unlines
    [ "######.",
      ".....#.",
      "######.",
      "#......",
      "######."
    ]

three :: String
three =
  unlines
    [ "######.",
      ".....#.",
      "...###.",
      ".....#.",
      "######."
    ]

four :: String
four =
  unlines
    [ "#......",
      "#......",
      "#...#..",
      "######.",
      "....#.."
    ]

five :: String
five =
  unlines
    [ "######.",
      "#......",
      "######.",
      ".....#.",
      "######."
    ]

six :: String
six =
  unlines
    [ "######.",
      "#......",
      "######.",
      "#....#.",
      "######."
    ]

seven :: String
seven =
  unlines
    [ "######.",
      ".....#.",
      ".....#.",
      ".....#.",
      ".....#."
    ]

eight :: String
eight =
  unlines
    [ "######.",
      "#....#.",
      "######.",
      "#....#.",
      "######."
    ]

nine :: String
nine =
  unlines
    [ "######.",
      "#....#.",
      "######.",
      ".....#.",
      "######."
    ]