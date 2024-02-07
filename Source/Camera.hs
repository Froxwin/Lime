module Camera where

import           Data.List                      ( sortBy )
import           Data.Maybe                     ( mapMaybe )
import           Ray                            ( Ray(Ray, direction) )
import           Vector                         ( (<+>)
                                                , (<->)
                                                , (<.>)
                                                , (</>)
                                                , Vector(Vector, vy)
                                                , unitVector
                                                )

aspectRatio :: Double
imgWidth, imgHeight :: Integer
aspectRatio = 16 / 9
imgWidth = 512
imgHeight = round $ fromInteger imgWidth / aspectRatio

viewportHeight, viewportWidth, focalLength :: Double
viewportHeight = 2
viewportWidth = aspectRatio * viewportHeight
focalLength = 1

cameraOrigin, horizontal, vertical, bottomLeft :: Vector
cameraOrigin = Vector 0 0 0
horizontal = Vector viewportWidth 0 0
vertical = Vector 0 viewportHeight 0
bottomLeft =
  cameraOrigin
    <-> (2 </> horizontal)
    <-> (2 </> vertical)
    <+> Vector 0 0 focalLength

-- List of xs and ys of viewport
u, v :: [Double]
u = map (/ fromIntegral imgWidth) [0 .. fromIntegral imgWidth - 1]
v = reverse
  $ map (/ fromIntegral imgHeight) [0 .. fromIntegral imgHeight - 1]

bottomColor, topColor :: Vector
bottomColor = Vector 1.0 1.0 1.0
topColor = Vector 0.5 0.7 1.0

type Object = Ray -> Double -> Double -> Maybe (Vector, Vector)

-- left to right top to bottom
rayColor :: Ray -> [Object] -> Color
rayColor r ls =
  let (Vector tx ty tz)
        | not (null hits) = 0.5 <.> (head hits <+> Vector 1 1 1)
        | otherwise       = ((1 - d) <.> bottomColor) <+> (d <.> topColor)
  in  Color tx ty tz
 where
  hitting ray tMin tMax f = f ray tMin tMax
  d    = 0.5 * (vy (unitVector $ direction r) + 1.0)
  hits = map fst $ sortBy (\(_, p1) (_, p2) -> compare p1 p2) $ mapMaybe
    (hitting r 0 (1 / 0))
    ls

samplesPerPixel :: Double
samplesPerPixel = 9
sampleSquare :: [Double]
sampleSquare =
  [-(sqrt samplesPerPixel - 1) / 2 .. (sqrt samplesPerPixel - 1) / 2]

render :: [Object] -> [Color]
render ls =
  [ foldl
      addColor
      (Color 0 0 0)
      [ rayColor (Ray cameraOrigin rayDirection) ls
      | sx <- sampleSquare
      , sy <- sampleSquare
      , let rayDirection =
              bottomLeft
                <+> ((ui + (sx / fromIntegral imgWidth)) <.> horizontal)
                <+> ((vi + (sy / fromIntegral imgWidth)) <.> vertical)
                <-> cameraOrigin
      ]
  | vi <- v
  , ui <- u
  ]

data Color = Color
  { red   :: Double
  , green :: Double
  , blue  :: Double
  }
  deriving Eq

instance Show Color where
  show :: Color -> String
  show (Color r g b) = concat $ (flip $ zipWith (++)) [" ", " ", ""] $ map
    (show . round . (/ samplesPerPixel) . (* 255.9))
    [r, g, b]

addColor :: Color -> Color -> Color
(Color r g b) `addColor` (Color r' g' b') =
  Color (r + r') (g + g') (b + b')
