module Main where

import qualified Personnummer

main :: IO ()
main = do
  if Personnummer.isValid pnr
    then
      putStrLn $
        "The person with personal identity number "
          ++ pnrFmt
          ++ " is a "
          ++ gender
          ++ " of age "
          ++ age
          ++ " at 2022-01-01"
    else putStrLn "Invalid personal identity number"
  where
    pnr = Personnummer.toPersonnummer "199001010017"
    pnrFmt = Personnummer.format pnr True
    gender = show $ Personnummer.gender pnr
    age = show $ Personnummer.getAgePure pnr (2022, 1, 1)
