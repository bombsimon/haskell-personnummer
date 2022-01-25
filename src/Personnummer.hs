module Personnummer
  ( luhn,
    valid,
    matchesToDate,
  )
where

import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    parseTimeOrError,
  )
import Text.Printf (printf)
import Text.Regex
  ( Regex,
    mkRegex,
  )

-- https://github.com/Tarrasch/threepenny-personnummer/blob/master/PersonNummer.hs

{-
Regex to match personal identity number. Used both to determine valid values but
also to extract relevant bits to be able to convert to proper date.
-}
pnrRe :: Regex
pnrRe = mkRegex "^(\\d{2})?(\\d{2})(\\d{2})(\\d{2})([-+]?)?(\\d{3})(\\d?)$"

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
    control :: Int,
    divider :: Char
  }

parseTime :: String -> UTCTime
parseTime = parseTimeOrError True defaultTimeLocale "%Y%m%d"

matchesToDate :: [Int] -> Maybe UTCTime
matchesToDate (century : year : month : day : xs) =
  Just $
    parseTime $
      show c
        ++ printf "%02d" year
        ++ printf "%02d" month
        ++ printf "%02d" day
  where
    c = if century == 0 then 19 else century
matchesToDate xs = Nothing

valid :: String -> Bool
valid raw = False

{-
Use luhn algoritm to calculate the control digit.
See more at https://en.wikipedia.org/wiki/Luhn_algorithm
-}
luhn :: [Int] -> Int
luhn =
  (10 -) . (`mod` 10) . sum . map (read . (: [])) . concatMap show
    . zipWith
      (*)
      (cycle [2, 1])
