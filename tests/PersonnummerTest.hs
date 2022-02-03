module Main where

import Personnummer
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import Test.HUnit
  ( Counts (errors, failures),
    Test (TestLabel, TestList),
    Testable (test),
    runTestTT,
    (~:),
    (~=?),
  )

validPersonnummer :: Test
validPersonnummer =
  test $
    map
      ( \(pnr, valid) ->
          "validate personnummer"
            ~: "(isValid (toPersonnummer \"" ++ pnr ++ "\"))"
            ~: valid
              ~=? isValid (toPersonnummer pnr)
      )
      [ ("not-parsable", False),
        ("6403273813", True),
        ("510818-9167", True),
        ("19900101-0017", True),
        ("19130401+2931", True),
        ("196408233234", True),
        ("0001010107", True),
        ("000101-0107", True),
        ("640327-381", False),
        ("6403273814", False),
        ("640327-3814", False),
        ("19090903-6600", True),
        ("20150916-0006", False)
      ]

checkGender :: Test
checkGender =
  test $
    map
      ( \(pnr, expectedGender) ->
          "get gender"
            ~: "(gender (toPersonnummer \"" ++ pnr ++ "\"))"
            ~: expectedGender
              ~=? gender (toPersonnummer pnr)
      )
      [ ("19090903-6600", Female),
        ("19900101-0017", Male),
        ("800101-3294", Male),
        ("000903-6609", Female),
        ("800101+3294", Male)
      ]

validCoordination :: Test
validCoordination =
  test $
    map
      ( \(pnr, expectedIsCoordination) ->
          "get gender"
            ~: "(isCoordination  (toPersonnummer \"" ++ pnr ++ "\"))"
            ~: expectedIsCoordination
              ~=? isCoordination (toPersonnummer pnr)
      )
      [ ("800161-3294", True),
        ("800101-3294", False),
        ("640327-3813", False)
      ]

getPersonAge :: Test
getPersonAge =
  test $
    map
      ( \(pnr, simulatedDate, expectedAge) ->
          "get age"
            ~: "(getAgePure (toPersonnummer \"" ++ pnr ++ "\") " ++ show simulatedDate ++ ")"
            ~: expectedAge
              ~=? getAgePure (toPersonnummer pnr) simulatedDate
      )
      [ ("199001010000", (2000, 1, 2), 10),
        ("199001020000", (2000, 1, 2), 10),
        ("199001030000", (2000, 1, 2), 9),
        ("189901010000", (2000, 1, 2), 101)
      ]

main :: IO Counts
main = do
  results <-
    runTestTT $
      TestList
        [ TestLabel "Valid personnummer" validPersonnummer,
          TestLabel "Check gender" checkGender,
          TestLabel "Valid coordination number" validCoordination,
          TestLabel "Test getting age" getPersonAge
        ]
  if errors results + failures results == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
