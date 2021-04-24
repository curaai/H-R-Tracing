module Main where
import qualified Data.ByteString.Char8 as C
import Img

main :: IO ()
main =
  C.writeFile "res.ppm" $
    C.pack $
      let size = Size 400 300 in toPpmStr . gradientImg $ size
