module Ray where 

import Vector
import Img


data Ray = Ray {origin :: Point, direction :: Vec} deriving (Show)

at :: Ray -> Float -> Point
at ray t = origin ray + direction ray *: t

