module Main where

import Personnummer (Personnummer, format, gender, getAge, isValid, toPersonnummer)

main :: IO ()
main = do
  case toPersonnummer "19900101-0017" of
    Just pnr -> showValidPnr pnr
    Nothing -> putStrLn "Invalid format"

showValidPnr :: Personnummer -> IO ()
showValidPnr pnr = do
  age <- getAge pnr
  if isValid pnr
    then
      putStrLn $
        "The person with personal identity number "
          ++ format pnr True
          ++ " is a "
          ++ show (gender pnr)
          ++ " of age "
          ++ show age
    else putStrLn "Invalid personal identity number"
