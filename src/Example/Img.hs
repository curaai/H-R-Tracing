{-# LANGUAGE TupleSections #-}

module Example.Img where

import Data.List

data Color = Color {r :: Int, g :: Int, b :: Int} deriving (Show)

data Size = Size {w :: Int, h :: Int}

gradientImg size = map (uncurry makePair) rows
  where
    makePair v lst = map (v,) lst
    rows = zip fYRange $ replicate (h size) fXRange :: [(Float, [Float])]
    fXRange = fRange size w
    fYRange = fRange size h
    fRange size sizeF = [0, 1 / ((\x -> fromIntegral x :: Float) . sizeF $ size) .. 1]

toPpmStr arr = header ++ concatMap (concatMap (color2str . toColor)) arr
  where
    header = unwords ["P3", show . w $ size, show . h $ size, "255\n"]
    size = Size (length arr) (length . head $ arr)
    color2str c = unwords [show . r $ c, show . g $ c, (show . b $ c) ++ "\n"]
    toColor p = Color red green blue
      where
        toColorInt = truncate . (* 255.99)
        red = toColorInt . fst $ p
        green = toColorInt . snd $ p
        blue = toColorInt 0.15
