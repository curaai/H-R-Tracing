module Ray where

import           Vector

data Ray =
  Ray
    { origin    :: Point
    , direction :: Vec
    }
  deriving (Show, Eq)

at :: Ray -> Float -> Point
at ray power = origin ray + direction ray *: power
