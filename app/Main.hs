module Main where

import qualified Personnummer

main :: IO ()
main = do
  age <- Personnummer.getAge pnr
  if Personnummer.isValid pnr
    then
      putStrLn $
        "The person with personal identity number "
          ++ pnrFmt
          ++ " is a "
          ++ gender
          ++ " of age "
          ++ show age
    else putStrLn "Invalid personal identity number"
  where
    pnr = Personnummer.toPersonnummer "199001010017"
    pnrFmt = Personnummer.format pnr True
    gender = show $ Personnummer.gender pnr
