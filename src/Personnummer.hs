module Personnummer
  ( Gender (Female, Male),
    Personnummer,
    control,
    date,
    divider,
    gender,
    getAge,
    getAgePure,
    isCoordination,
    isFemale,
    isMale,
    isValid,
    format,
    number,
    toPersonnummer,
  )
where

import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
    parseTimeM,
    toGregorian,
  )
import Data.Time.Clock (utctDay)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex
  ( Regex,
    matchRegex,
    mkRegex,
  )

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

{-
Personnummber represents all the date for a personal identity number. The birth
date is represented as a UTCTime for easier handling. Will be Nothing if not a
valid date. Even if the Personnummer is a coordination number we store a proper
date and set isCoordination to True instead.
-}
data Personnummer = Personnummer
  { date :: Maybe UTCTime,
    number :: Int,
    control :: Int, -- Always three digits but may lead with zero.
    divider :: Char,
    isCoordination :: Bool
  }
  deriving (Eq, Show)

{-
Check if a Personnummber is valid. That is, if it is a valid date and the
checksum adds up to the control digit.
-}
isValid :: Personnummer -> Bool
isValid p
  | isNothing (date p) = False
  | number p == 0 = False
  | luhnP p == control p = True
  | otherwise = False

{-
Get the gender for the Personnummber.
-}
gender :: Personnummer -> Gender
gender p = case mod (mod (number p) 10) 2 of
  0 -> Female
  _ -> Male

{-
Check if the Personnummber belongs to a Female.
-}
isFemale :: Personnummer -> Bool
isFemale p = gender p == Female

{-
Check if the Personnummber belongs to a Male.
-}
isMale :: Personnummer -> Bool
isMale p = gender p == Male

{-
Get the age based on the current time.
-}
getAge :: Personnummer -> IO Int
getAge p =
  do
    now <- getCurrentTime
    let (y, m, d) = toGregorian $ utctDay now
    let age = getAgePure p (fromIntegral y, m, d)
    return age

{-
Get age pure doesn't calculate the current time but instead asks the caller to
send a tuple with the current year, month and day. Based on that the functon
will calculate the age.
-}
getAgePure :: Personnummer -> (Int, Int, Int) -> Int
getAgePure p (y, m, d)
  | isNothing $ date p = 0
  | m > m2 = y - y2
  | m == m2 && d >= d2 = y - y2
  | otherwise = y - y2 - 1
  where
    (year, m2, d2) = toGregorian $ utctDay (fromJust $ date p)
    y2 = fromIntegral year

{-
Format to consistent Personnummer output, either in long format (including
century) or short format (just two digits)
-}
format :: Personnummer -> Bool -> String
format p long =
  t
    ++ [divider p]
    ++ printf "%03d" (number p)
    ++ printf "%d" (control p)
  where
    f = if long then "%Y%m%d" else "%y%m%d"
    t = case date p of
      Just d -> formatTime defaultTimeLocale f d
      Nothing -> ""

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
      luhn $
        map (read . (: [])) $
          formatTime defaultTimeLocale "%y%m%d" t ++ printf "%03d" (number p)

{-
Use luhn algoritm to calculate the control digit.
-}
luhn :: [Int] -> Int
luhn =
  (`mod` 10)
    . (10 -)
    . sum
    . map (read . (: []))
    . concatMap show
    . zipWith
      (*)
      (cycle [2, 1])
