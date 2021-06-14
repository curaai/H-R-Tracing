module Hittable.HittableList where

import           Data.Maybe        (isNothing)
import           Hit               (HitRange (HitRange, hitTMax, hitTMin),
                                    HitRecord (hitT))
import           Hittable.Hittable (Hittable (..))

instance (Hittable a) => Hittable [a] where
  hit a ray rr = foldl f Nothing a
    where
      f :: Hittable a => Maybe HitRecord -> a -> Maybe HitRecord
      f hr obj =
        let res = hit obj ray (rr' hr)
         in if isNothing res
              then hr
              else res
        where
          rr' hr = HitRange (hitTMin rr) (maybe (hitTMax rr) hitT hr)
