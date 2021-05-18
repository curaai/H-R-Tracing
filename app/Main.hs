module Main where

import qualified Data.ByteString.Char8 as C

import           Camera
import           Vector

aspectRatio = 16 / 9

drawImg :: (Show a, Integral a) => Size a -> String
drawImg size = unlines $ "P3" : size' : "255" : map (show . writeColor) arr
  where
    arr = render cam $ map relative coords
      where
        cam =
          Camera
            (Size (aspectRatio * viewportHeight) viewportHeight)
            (pure 0)
            (Vec3 0 0 focalLength')
            focalLength'
          where
            viewportHeight = 2
            focalLength' = 1
        coords =
          (,) <$> reverse [0 .. height size - 1] <*> [0 .. width size - 1]
        relative (y, x) = (func width x, func height y)
          where
            func f v = toFloat v / ((+ (-1)) . toFloat . f) size
            toFloat x = fromIntegral x :: Float
    writeColor = fmap (truncate . (* 255.999))
    size' = unwords . map show $ [width size, height size]

main :: IO ()
main =
  C.writeFile "res.ppm" . C.pack $
  drawImg (Size 400 (truncate (400 / aspectRatio)))
