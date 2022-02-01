module Main where

import Personnummer (isValid, toPersonnummer)
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "invalid input" ~: "(isValid (toPersonnummer \"not-parsable\"))" ~: False ~=? isValid (toPersonnummer "not-parsable"),
      "invalid date" ~: "(isValid (toPersonnummer \"199001410017\"))" ~: False ~=? isValid (toPersonnummer "199001410017"),
    ]

main :: IO Counts
main = do
  runTestTT tests'
