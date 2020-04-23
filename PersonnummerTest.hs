module Main where

import qualified Personnummer (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Personnummer.someFunc
