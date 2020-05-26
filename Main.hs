module Main where

import Personnummer (luhn, valid)

getStr :: String
getStr = "Hello There"

main :: IO ()
main =
    print $ Personnummer.luhn [9,0,0,1,0,1,0,0,1]
