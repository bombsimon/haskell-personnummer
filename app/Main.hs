module Main where

import qualified Personnummer

main :: IO ()
main = do
  -- putStrLn $ "Is " ++ n ++ " valid? Result: " ++ show (Personnummer.valid n)
  -- where n = "19900101-0001"

  putStrLn $ "Test: " ++ toStr (Personnummer.matchesToDate [20, 20, 2, 29])
  where
    toStr x = case x of
      Just x -> show x
      Nothing -> show "No dice :("
