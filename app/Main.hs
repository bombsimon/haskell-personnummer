module Main where

import qualified Personnummer (valid)

main :: IO ()
main = do
  putStrLn $ "Is " ++ n ++ " valid? Result: " ++ show (Personnummer.valid n)
  where
    n = "19900101-0001"
