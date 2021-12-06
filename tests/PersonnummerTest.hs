module Main where

import Personnummer (valid)
import Test.HUnit

tests' :: Test
tests' =
  test
    [ "invalid input" ~: "(valid \"not-parsable\")" ~: False ~=? Personnummer.valid "not-parsable",
      "invalid date" ~: "(valid \"199001410017\")" ~: False ~=? Personnummer.valid "199001410017"
    ]

main :: IO Counts
main = do
  runTestTT tests'
