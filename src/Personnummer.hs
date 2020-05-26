module Personnummer (luhn, valid) where

-- https://github.com/Tarrasch/threepenny-personnummer/blob/master/PersonNummer.hs

data Gender = Male | Female
  deriving (Eq, Show)

charToInt :: Char -> Int
charToInt = read . (:[])

valid :: String -> Bool
valid raw =
        False

luhn :: [Int] -> Int
luhn = (10-)
     . (`mod` 10)
     . sum
     . map charToInt
     . concatMap show
     . zipWith (*) (cycle [2,1])
