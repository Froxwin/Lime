module Main where

import           Camera
import           Color
import           Data.Maybe
import           Objects.Hittable
import           Objects.Sphere
import           Ray
import           System.IO
import           Vector

u, v :: [Double]
u = map (/ imgWidthD) [0 .. imgWidthD - 1]
v = reverse $ map (/ imgHeightD) [0 .. imgHeightD - 1]

bottomColor, topColor :: Vector3
bottomColor = Vector3 1.0 1.0 1.0
topColor = Vector3 0.5 0.7 1.0

rayColor :: Ray -> Color
rayColor r =
  let (Vector3 tx ty tz)
        | isJust sphere
        = 0.6 <.> (hitNormal (fromJust sphere) <+> Vector3 1 1 1)
        | isJust sphere'
        = 0.6 <.> (hitNormal (fromJust sphere') <+> Vector3 1 1 1)
        | otherwise
        = ((1 - d) <.> bottomColor) <+> (d <.> topColor)
  in  Color tx ty tz
 where
  d       = 0.5 * (vy (unitVector $ direction r) + 1.0)
  sphere  = hit (Sphere (Vector3 0 10 50) 20) r 0 (1 / 0)
  sphere' = hit (Sphere (Vector3 (-5) (-10) 50) 10) r 0 (1 / 0)

rayList :: [Color]
rayList =
  [ rayColor (Ray cameraOrigin rayDirection)
  | vi <- v
  , ui <- u
  , let rayDirection =
          bottomLeft
            <+> (ui <.> horizontal)
            <+> (vi <.> vertical)
            <-> cameraOrigin
  ]

main :: IO ()
main = do
  (_, handle) <- openTempFile "." "temp.ppm"
  hPutStrLn handle
    $ concat ["P3\n", show imgWidth, " ", show imgHeight, " 255\n"]
  mapM_ (hPrint handle) rayList
  hClose handle
