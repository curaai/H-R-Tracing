module Hitable where 

import Ray 
import Vector

class Hitable a where 
    isHit :: a -> Ray -> Bool
    
data Sphere = Sphere {center :: Point, radius :: Float}

instance Hitable Sphere where 
  isHit s r = (b*b - 4*a*c) > 0
    where
      oc = origin r - center s
      a = let dir = direction r in vDot dir dir 
      b = 2 * vDot oc (direction r)
      c = vDot oc oc - let radius' = radius s in radius' * radius'
