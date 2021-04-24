{-# LANGUAGE TupleSections #-}

module Img where


import Data.List
import Vec

data Color = Color {r :: Int, g :: Int, b :: Int} deriving (Show, Eq)

data Size = Size {w :: Int, h :: Int} deriving(Show, Eq)

vec2color v = Color (_x cv) (_y cv) (_z cv)
  where
    cv = fmap (truncate . (* 255.99)) v

gradientImg size = map (map pair2vec) img
  where
    img = map (uncurry makePair) rows
    pair2vec p = uncurry Vec3 p blue
    makePair y = map (y,)
    blue = 0.15
    rows = zip fYRange $ replicate (h size) fXRange :: [(Float, [Float])]
    fXRange = fRange . w $ size
    fYRange = fRange . h $ size
    fRange n = init [0, 1/(fromIntegral n :: Float) .. 1]

toPpmStr arr = header ++ concatMap (concatMap (color2str . vec2color)) arr
  where
    header = unwords ["P3", show . w $ size, show . h $ size, "255\n"]
    size = Size (length . head $ arr) (length arr)
    color2str c = unwords [show . r $ c, show . g $ c, (show . b $ c) ++ "\n"]
