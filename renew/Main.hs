module Main where

import qualified Data.ByteString.Char8 as C

import           Vector

data Size =
  Size
    { width  :: Int
    , height :: Int
    }

makeSimplePPM :: Size -> String
makeSimplePPM size =
  let coords = (,) <$> reverse [0 .. height size - 1] <*> [0 .. width size - 1]
   in unlines $ "P3" : size' : "255" : map (show . toColor) coords
  where
    size' = unwords . map show $ [width size, height size]
    toColor (y, x) =
      truncate . (* 255.999) <$>
      Vec3
        (toFloat x / ((+ (-1)) . toFloat . width) size)
        (toFloat y / ((+ (-1)) . toFloat . height) size)
        0.25
      where
        toFloat x = fromIntegral x :: Float

main :: IO ()
main = C.writeFile "res.ppm" . C.pack $ makeSimplePPM (Size 256 256)