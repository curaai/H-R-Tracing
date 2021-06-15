module Hittable.HittableList where

import           Data.Bool         (bool)
import           Data.Maybe        (isNothing)
import           Hit               (HitRange (HitRange, hitTMax, hitTMin),
                                    HitRecord (hitT))
import           Hittable.Hittable (Hittable (..))

instance (Hittable a) => Hittable [a] where
  hit a ray rr = foldl f Nothing a
    where
      f :: Hittable a => Maybe HitRecord -> a -> Maybe HitRecord
      f hr obj = bool res hr (isNothing res)
        where
          rr' = rr {hitTMax = maybe (hitTMax rr) hitT hr}
          res = hit obj ray rr'
