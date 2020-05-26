module Main where

import Personnummer (valid)
import Test.HUnit

invalidInput :: Test
invalidInput =
    TestCase (assertEqual "invalid input (not parsable)" False (Personnummer.valid "not-parsable"))

invalidDate :: Bool
invalidDate =
    TestCase (assertEqual "invalid date (not parsable)" False (Personnummer.valid "199001410017"))

tests :: Test
tests = TestList [TestLabel "invalid input" invalidInput, TestLabel "invalid date" invalidDate]

tests' :: Test
tests' = test [ "invalid input" ~: "(valid \"not-parsable\")" ~: False ~=? Personnummer.valid "not-parsable",
                "invalid date" ~: "(valid \"199001410017\")" ~: False ~=? Personnummer.valid "199001410017" ]

main :: IO Counts
main = do _ <- runTestTT tests
          runTestTT tests'
