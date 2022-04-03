module MyLib (someFunc) where

import RIO
import RIO.ByteString.Lazy

someFunc :: IO ()
someFunc = putStrLn "someFunc"
