module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

magicCoin = \x -> True 

-- >>> magicCoin 1
-- True
