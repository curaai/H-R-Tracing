{-# LANGUAGE TupleSections #-}

module Img where

import           Camera
import           Data.List
import           Vector

aspectRatio s = (toFloat . w) s / (toFloat . h) s
  where
    toFloat = \x -> fromIntegral x :: Float

gradientImg size = map (map pair2vec) img
  where
    pair2vec p = uncurry Vec3 p blue
    img = map (uncurry makePair) rows
    makePair y = map (y, )
    blue = 0.15
    rows = zip fYRange $ replicate (h size) fXRange :: [(Float, [Float])]
    fXRange = fRange . w $ size
    fYRange = fRange . h $ size
    fRange n = init [0,1 / (fromIntegral n :: Float) .. 1]

toPpmStr :: [[Color]] -> [Char]
toPpmStr arr = header ++ concatMap (concatMap color2str) arr
  where
    header = unwords ["P3", show . w $ size, show . h $ size, "255\n"]
    size = Size (length . head $ arr) (length arr)
    color2str c =
      unwords [show . _r $ c, show . _g $ c, (show . _b $ c) ++ "\n"]
