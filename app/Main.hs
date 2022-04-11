module Main where

import RIO
import qualified Core (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Core.someFunc
