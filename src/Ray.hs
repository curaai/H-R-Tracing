module Ray where

import           Img
import           Vector

data Ray =
  Ray
    { origin    :: Point
    , direction :: Vec
    }
  deriving (Show)

at :: Ray -> Float -> Point
at ray t = origin ray + direction ray *: t
