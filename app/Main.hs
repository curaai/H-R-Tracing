module Main where

import qualified Data.ByteString.Char8 as C

data Color = Color {r :: Int, g :: Int, b :: Int}

data Size = Size {w :: Int, h :: Int}

arr = replicate (h size) [1 .. w size]

size = Size 400 300

toFloat x = fromIntegral x :: Float

colorRow y row = map (\x -> Color x green blue) $ mapColor (w size) row
  where
    toColorInt = truncate . (* 255.99)
    blue = toColorInt 0.15
    green = toColorInt . normalize (h size) $ y
    mapColor max' = map (toColorInt . normalize max')
    normalize max' x = toFloat x / toFloat max'

imgRes = concatMap (concatMap color2str) img
    where 
        img = zipWith colorRow [0 ..] arr
        color2str c = (show . r $ c) ++ " " ++ (show . g $ c) ++ " " ++ (show . b $ c) ++ "\n" 

res = "P3 " ++ (show . w $ size) ++ " " ++ (show . h $ size) ++ " 255\n" ++ imgRes

main :: IO ()
main = C.writeFile "res.ppm" $ C.pack res