module Ray where 

import Vector
import Img

type Point = Vec3 Float 

data Ray = Ray {origin :: Point, direction :: Vec} deriving (Show)

at :: Ray -> Float -> Point
at ray t = origin ray + direction ray *: t

ray2color r = a *: (1.0 - t) + b *: t
  where 
    a = Vec3 1.0 1.0 1.0
    b = Vec3 0.5 0.7 1.0
    t = 0.5 * (_y unitDir + 1.0)
    unitDir = vUnit . direction $ r
