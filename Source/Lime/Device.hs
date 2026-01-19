module Lime.Device where

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BI
import Foreign
import Foreign.C
-- import Paths_Lime

--loadPTX :: IO FilePath
--loadPTX = getDataFileName "build/kernel.ptx"

--foreign import ccall "print_gpu_info"
--  c_print_gpu_info :: IO CInt

--printGpuInfo :: IO Int
--printGpuInfo = do
--  r <- c_print_gpu_info
--  return (fromIntegral r)

--foreign import ccall "launch_render"
--  c_launchRender :: CString -> Ptr Word8 -> CInt -> CInt -> IO CInt

--launchRender :: String -> Ptr Word8 -> Int -> Int -> IO Int
--launchRender ptx pixels w h =
--  withCString ptx $ \cptx -> do
--    r <- c_launchRender cptx pixels (fromIntegral w) (fromIntegral h)
--    return (fromIntegral r)

savePPM :: Ptr Word8 -> Int -> Int -> IO ()
savePPM pixels width height = do
  let size = width * height * 3
  header <- pure $ "P6\n" ++ show width ++ " " ++ show height ++ "\n255\n"
  BS.writeFile "out.ppm" =<< do
    pixelBytes <- BS.packCStringLen (castPtr pixels, size)
    pure $ BS.append (BS.pack (map (fromIntegral . fromEnum) header)) pixelBytes

hmm = do
--  c_print_gpu_info
--  ptx <- getDataFileName "build/kernel.ptx" >>= readFile
--  let (width, height) = (1920, 1080)
--  let size = width * height * 3
--  pixels <- mallocBytes size :: IO (Ptr Word8)
--  _ <- launchRender ptx pixels width height
--  savePPM pixels width height
--  free pixels
  undefined
