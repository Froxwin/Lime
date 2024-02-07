module Camera where

import           Vector                         ( Vector
                                                  ( (<+>)
                                                  , (<->)
                                                  , (</>)
                                                  )
                                                , Vector3(Vector3)
                                                )

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
