module Personnummer (isValid, toPersonnummer) where

import Data.Maybe (fromMaybe, isNothing)
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    formatTime,
    parseTimeM,
    parseTimeOrError,
  )
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex
  ( Regex,
    matchRegex,
    mkRegex,
  )

-- https://github.com/Tarrasch/threepenny-personnummer/blob/master/PersonNummer.hs

{-
Regex to match personal identity number. Used both to determine valid values but
also to extract relevant bits to be able to convert to proper date.
-}
pnrRe :: Regex
pnrRe = mkRegex "^([0-9]{2})?([0-9]{2})([0-9]{2})([0-9]{2})([-+])?([0-9]{3})([0-9])?$"

{-
Gender can be either Maile or Female.
-}
data Gender
  = Male
  | Female
  deriving (Eq, Show)

data Personnummer = Personnummer
  { date :: Maybe UTCTime,
    number :: Int,
    control :: Int, -- Always three digits but may lead with zero.
    divider :: Char,
    isCoordination :: Bool
  }

{-
Construct a Personnummer from a string.
-}
toPersonnummer :: String -> Personnummer
toPersonnummer s =
  maybe
    Personnummer
      { date = Nothing,
        number = 0,
        control = 0,
        divider = '-',
        isCoordination = False
      }
    toPersonnummerFrommatches
    $ matchRegex pnrRe s

{-
Construct a Personnummer when we know we have valid matches.
-}
toPersonnummerFrommatches :: [String] -> Personnummer
toPersonnummerFrommatches m =
  Personnummer
    { date = matchesToDate x,
      number = x !! 5,
      control = x !! 6,
      divider = d,
      isCoordination = x !! 3 > 31
    }
  where
    x = map intOrZeroFromString m
    d = if m !! 4 == "+" then '+' else '-'

isValid :: Personnummer -> Bool
isValid p
  | isNothing (date p) = False
  | luhnP p == control p = True
  | otherwise = False

{-
Try to read string as int and if it fails, return 0
-}
intOrZeroFromString :: String -> Int
intOrZeroFromString s = fromMaybe 0 (readMaybe s)

{-
Convert a list of matches from a parsed date to UTCTime if the time is valid.
Input should contain at least [ century, year, month, day ] but may contain
control number and checksum as well.
-}
matchesToDate :: [Int] -> Maybe UTCTime
matchesToDate (century : year : month : day : xs) =
  parseTime $
    show c
      ++ printf "%02d" year
      ++ printf "%02d" month
      ++ printf "%02d" (day `mod` 60)
  where
    c = if century == 0 then 19 else century
matchesToDate xs = Nothing

{-
Convert a string to a UTCTime if the string is a valid %Y%m%d string
-}
parseTime :: String -> Maybe UTCTime
parseTime s =
  case parseTimeM True defaultTimeLocale "%Y%m%d" s of
    [t] -> Just t
    _ -> Nothing

{-
Use luhn algoritm to calculate the control digit.
See more at https://en.wikipedia.org/wiki/Luhn_algorithm
-}
luhnP :: Personnummer -> Int
luhnP p =
  case date p of
    Nothing -> -1
    Just t ->
      luhn $ map (read . (: [])) $ d ++ printf "%03d" (number p)
      where
        d = formatTime defaultTimeLocale "%y%m%d" t

{-
Use luhn algoritm to calculate the control digit.
-}
luhn :: [Int] -> Int
luhn =
  (10 -)
    . (`mod` 10)
    . sum
    . map (read . (: []))
    . concatMap show
    . zipWith
      (*)
      (cycle [2, 1])
