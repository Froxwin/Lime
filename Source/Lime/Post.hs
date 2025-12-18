module Lime.Post where

import Data.Bifunctor
import Data.Color
import Data.Vector qualified as V
import Lime.Camera

-- type Filter = Render -> Render

-- boxBlur amt (Render width height image) =
--   Render
--     width
--     height
--     ( V.imap
--         ( \i _ ->
--             let (x, y) = here i
--                 a =
--                   (\t -> (1 / fromIntegral (length kernel)) `scale` t) $
--                     getColorSum $
--                       mconcat $
--                         map (ColorSum . uncurry pix . bimap (x +) (y +)) kernel
--              in a
--         )
--         image
--     )
--  where
--   kernel = [(x, y) | x <- [-amt .. amt], y <- [-amt .. amt]]
--   here i =
--     let x = i `mod` width
--         y = (i - x) `div` width
--      in (x, y)
--   pix x y =
--     if x <= 0 || y <= 0 || x >= width || y >= height
--       then pure 0
--       else image V.! ((width * y) + x)

-- bpf :: Render -> Render
-- bpf (Render width height image) =
--   Render
--     width
--     height
--     ( V.imap
--         ( \_ p@(Color r g b) ->
--             if sqrt ((r ^ 2) + (g ^ 2) + (b ^ 2)) >= 0.9 then p else pure 0
--         )
--         image
--     )

-- bloom r@(Render width height image) =
--   Render
--     width
--     height
--     ( V.imap
--         ( \i p ->
--             let b = 1 `scale` (uncurry pix (here i))
--              in (1 / 2) `scale` (getColorSum $ (ColorSum p) <> (ColorSum b))
--         )
--         image
--     )
--  where
--   (Render _ _ bright) =
--     boxBlur 13 $
--       boxBlur 11 $
--         boxBlur 9 $
--           boxBlur 7 $
--             boxBlur 5 $
--               boxBlur 3 $
--                 bpf r
--   here i =
--     let x = i `mod` width
--         y = (i - x) `div` width
--      in (x, y)
--   pix x y =
--     if x <= 0 || y <= 0 || x >= width || y >= height
--       then pure 0
--       else bright V.! ((width * y) + x)

-- https://blog.demofox.org/2018/07/04/pathtraced-depth-of-field-bokeh/
-- https://learnopengl.com/Guest-Articles/2022/Phys.-Based-Bloom

-- downsample (Render width height image) = Render
--   (round $ fromIntegral width / 2)
--   (round $ fromIntegral height / 2)
--   (V.imap (\i _ -> weighted (here i)) image)
--  where
--   kernel05 (x, y) =
--     [(x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)]
--   kernel025 (x, y) = [(x, y - 2), (x, y + 2), (x - 2, y), (x + 2, y)]
--   kernel0125 (x, y) =
--     [(x - 2, y - 2), (x + 2, y - 2), (x - 2, y + 2), (x + 2, y + 2)]
--   kernel1 (x, y) = [(x, y)]
--   weighted p = (\t -> (1 / 13) `scale` t) $ getColorSum $ mconcat $ map ColorSum $ concatMap
--     (\(q, t) -> map ((q `scale`) . uncurry pix) $ t p)
--     [(1, kernel1), (0.5, kernel05), (0.25, kernel025), (0.125, kernel0125)]
--   here i =
--     let x = i `mod` width
--         y = (i - x) `div` width
--     in  (x, y)
--   pix x y = if x <= 0 || y <= 0 || x >= width || y >= height
--     then pure 0
--     else image V.! ((width * y) + x)
