module Main where

import           Camera                         ( bottomLeft
                                                , cameraOrigin
                                                , horizontal
                                                , imgHeight
                                                , imgHeightD
                                                , imgWidth
                                                , imgWidthD
                                                , vertical
                                                )
import           Color                          ( Color(Color) )
import           Data.List                      ( sortBy )
import           Data.Maybe                     ( mapMaybe )
import           Objects.Sphere                 ( sphere )
import           Ray                            ( Ray(Ray, direction) )
import           System.IO                      ( hClose
                                                , hPrint
                                                , hPutStrLn
                                                , openTempFile
                                                )
import           Vector                         ( Vector
                                                  ( (<+>)
                                                  , (<->)
                                                  , (<.>)
                                                  )
                                                , Vector3(Vector3, vy)
                                                , unitVector
                                                )

u, v :: [Double]
u = map (/ imgWidthD) [0 .. imgWidthD - 1]
v = reverse $ map (/ imgHeightD) [0 .. imgHeightD - 1]

bottomColor, topColor :: Vector3
bottomColor = Vector3 1.0 1.0 1.0
topColor = Vector3 0.5 0.7 1.0

objects :: [Ray -> Double -> Double -> Maybe (Vector3, Vector3)]
objects =
  [ sphere (Vector3 (-10) 0 80)   50
  , sphere (Vector3 0 (-100.5) 0) 100
  , sphere (Vector3 30 10 60)     20
  , sphere (Vector3 0 0 5)        2
  ]

hitting :: t1 -> t2 -> t3 -> (t1 -> t2 -> t3 -> t4) -> t4
hitting ray tMin tMax f = f ray tMin tMax

rayColor
  :: (Ord a, Fractional t3, Num t2)
  => Ray
  -> [Ray -> t2 -> t3 -> Maybe (Vector3, a)]
  -> Color
rayColor r ls =
  let (Vector3 tx ty tz)
        | not (null thing) = 0.5 <.> (head thing <+> Vector3 1 1 1)
        | otherwise        = ((1 - d) <.> bottomColor) <+> (d <.> topColor)
  in  Color tx ty tz
 where
  d     = 0.5 * (vy (unitVector $ direction r) + 1.0)
  thing = map fst $ sortBy (\(_, p1) (_, p2) -> compare p1 p2) $ mapMaybe
    (hitting r 0 (1 / 0))
    ls

rayList :: [Color]
rayList =
  [ rayColor (Ray cameraOrigin rayDirection) objects
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
  (p, handle) <- openTempFile "." "temp.ppm"
  hPutStrLn handle
    $ concat ["P3\n", show imgWidth, " ", show imgHeight, " 255\n"]
  mapM_ (hPrint handle) rayList
  hClose handle
  putStrLn $ "[ \x1b[32mSaved to " ++ p ++ "\x1b[0m ]"
