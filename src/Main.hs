module Main where

import           Color
import           Data.Maybe
import           Hittable
import           Ray
import           Vector

aspectRatio, imgWidthD, imgHeightD :: Double
imgWidth, imgHeight :: Integer
aspectRatio = 16 / 9
imgWidth = 600
imgHeight = round $ fromInteger imgWidth / aspectRatio
imgWidthD = fromIntegral imgWidth
imgHeightD = fromIntegral imgHeight

viewportHeight, viewportWidth, focalLength :: Double
viewportHeight = 2
viewportWidth = aspectRatio * viewportHeight
focalLength = 1

cameraOrigin, horizontal, vertical, bottomLeft :: Vector3
cameraOrigin = Vector3 0 0 0
horizontal = Vector3 viewportWidth 0 0
vertical = Vector3 0 viewportHeight 0
bottomLeft =
  cameraOrigin
    <-> (horizontal </> 2)
    <-> (vertical </> 2)
    <+> Vector3 0 0 focalLength

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
        = 0.6 <.> (normal (fromJust sphere) <+> Vector3 1 1 1)
        | isJust sphere'
        = 0.6 <.> (normal (fromJust sphere') <+> Vector3 1 1 1)
        | otherwise
        = ((1 - d) <.> bottomColor) <+> (d <.> topColor)
  in  Color tx ty tz
 where
  d       = 0.5 * (vy (unitVector $ direction r) + 1.0)
  sphere  = hit (Sphere (Vector3 0 10 50) 20) r 0 (1 / 0)
  sphere' = hit (Sphere (Vector3 (-5) (-10) 50) 10) r 0 (1 / 0)

colorList :: [Color]
colorList = map rayColor rayList

main :: IO ()
main = do
  putStrLn $ concat ["P3\n", show imgWidth, " ", show imgHeight, " 255\n"]
  -- mapM_ print colorList

rayList :: [Ray]
rayList =
  [ Ray cameraOrigin rayDirection
  | vi <- v
  , ui <- u
  , let rayDirection =
          bottomLeft
            <+> (ui <.> horizontal)
            <+> (vi <.> vertical)
            <-> cameraOrigin
  ]
