module Camera where

import           Color                          ( Color(Color)
                                                , addColor
                                                )
import           Data.List                      ( sortBy )
import           Data.Maybe                     ( mapMaybe )
import           Ray                            ( Ray(Ray, direction) )
import           System.Random
import           Vector                         ( Vector
                                                  ( (<+>)
                                                  , (<->)
                                                  , (<.>)
                                                  , (</>)
                                                  )
                                                , Vector3(Vector3, vy)
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

cameraOrigin, horizontal, vertical, bottomLeft :: Vector3
cameraOrigin = Vector3 0 0 0
horizontal = Vector3 viewportWidth 0 0
vertical = Vector3 0 viewportHeight 0
bottomLeft =
  cameraOrigin
    <-> (horizontal </> 2)
    <-> (vertical </> 2)
    <+> Vector3 0 0 focalLength

-- List of xs and ys of viewport
u, v :: [Double]
u = map (/ fromIntegral imgWidth) [0 .. fromIntegral imgWidth - 1]
v = reverse
  $ map (/ fromIntegral imgHeight) [0 .. fromIntegral imgHeight - 1]

bottomColor, topColor :: Vector3
bottomColor = Vector3 1.0 1.0 1.0
topColor = Vector3 0.5 0.7 1.0

type Object = Ray -> Double -> Double -> Maybe (Vector3, Vector3)

-- left to right top to bottom
rayColor :: Ray -> [Object] -> Color
rayColor r ls =
  let (Vector3 tx ty tz)
        | not (null hits) = 0.5 <.> (head hits <+> Vector3 1 1 1)
        | otherwise       = ((1 - d) <.> bottomColor) <+> (d <.> topColor)
  in  Color tx ty tz
 where
  hitting ray tMin tMax f = f ray tMin tMax
  d    = 0.5 * (vy (unitVector $ direction r) + 1.0)
  hits = map fst $ sortBy (\(_, p1) (_, p2) -> compare p1 p2) $ mapMaybe
    (hitting r 0 (1 / 0))
    ls

render :: Monad m => [Object] -> m [Color]
render ls = do
  return
    [ foldl
        addColor
        (Color 0 0 0)
        [ rayColor (Ray cameraOrigin rayDirection) ls
        | sx <- [-1 .. 1]
        , sy <- [-1 .. 1]
        , let
          rayDirection =
            bottomLeft
              <+> (   (ui + (fromIntegral sx / fromIntegral imgWidth))
                  <.> horizontal
                  )
              <+> (   (vi + (fromIntegral sy / fromIntegral imgWidth))
                  <.> vertical
                  )
              <-> cameraOrigin
        ]
    | vi <- v
    , ui <- u
    ]

randomIntInRange :: Int -> Int -> IO Int
randomIntInRange minVal maxVal = do
  a <- randomRIO (minVal, maxVal)
  print a
  return a

main :: IO ()
main = do
  randomNumber <- randomIntInRange (-4) 4
  putStrLn $ "Random number between -4 and 4: " ++ show randomNumber

-- render :: [Object] -> [Color]
-- render ls =
--   [ rayColor (Ray cameraOrigin rayDirection) ls
--   | vi <- v
--   , ui <- u
--   , let rayDirection =
--           bottomLeft
--             <+> (ui <.> horizontal)
--             <+> (vi <.> vertical)
--             <-> cameraOrigin
--   ]

