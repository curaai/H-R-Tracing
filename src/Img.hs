module Img where

import           Camera
import           Data.List
import           Sampling
import           System.Random
import           Vector

data Image =
  Image
    { size   :: Size Int
    , colors :: [Color]
    }

aspectRatio s = (toFloat . width) s / (toFloat . height) s
  where
    toFloat = \x -> fromIntegral x :: Float

toPpmStr :: Image -> [Char]
toPpmStr img = header ++ concatMap color2str (colors img)
  where
    header =
      unwords
        ["P3", show . width . size $ img, show . height . size $ img, "255\n"]
    color2str c =
      unwords [show . _r $ c, show . _g $ c, (show . _b $ c) ++ "\n"]

make2d :: (Num b, Enum b) => Size b -> [(b, b)]
make2d size = [(i, j) | j <- reverse . range' $ height, i <- range' width]
  where
    range' f = [0,1 .. f size - 1]

screen cam spheres = Image (imageSize cam) (map computeColor img')
  where
    img' = make2d . imageSize $ cam
    computeColor (x, y) =
      vec2color' . fst $
      foldl sampling (Vec3 0 0 0, g) [1 .. samplePerPixel cam]
      where
        sampling :: RandomGen g => (Vec3 Float, g) -> a -> (Vec3 Float, g)
        sampling (acc, g') _ = (acc + color, g3)
          where
            size = imageSize cam
            (u, v) =
              ( (toFloat x + i) / (toFloat . width . imageSize $ cam)
              , (toFloat y + j) / (toFloat . height . imageSize $ cam))
            (i, g1) = sampleFloat g'
            (j, g2) = sampleFloat g1
            (color, g3) = rayColor spheres (posToRay' (u, v)) 50 g2
        vec2color' = vec2color $ toFloat . samplePerPixel $ cam
        posToRay' = posToRay cam
        g = mkStdGen $ x * (width . imageSize $ cam) + y
