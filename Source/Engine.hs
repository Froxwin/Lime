module Engine where

import           Camera                         ( Scene(height, width)
                                                , render
                                                )
import           Data.List                      ( intercalate )
import           Data.Yaml                      ( decodeFileEither )
import           System.Exit                    ( ExitCode(ExitSuccess) )
import           System.IO                      ( hClose
                                                , hGetContents
                                                , hPutStr
                                                )
import           System.Process                 ( CreateProcess(std_in, std_out)
                                                , StdStream(CreatePipe)
                                                , createProcess
                                                , proc
                                                , waitForProcess
                                                )

-- | Generates content for the ppm file
makeImageFile :: Scene -> String
makeImageFile scene =
  concat ["P3\n", show (width scene), " ", show (height scene), " 255\n"]
    ++ intercalate "\n" (map show (render scene))

-- | Ignites the engine
ignite :: String -> String -> Bool -> IO ()
ignite input output force = do
  scene <- either (error . show) id <$> decodeFileEither input
  let options =
        ["-hide_banner", "-loglevel", "error", "-i", "-", output]
          ++ [ "-y" | force ]
  (Just hIn, Just hOut, _, handle) <- createProcess (proc "ffmpeg" options)
    { std_in  = CreatePipe
    , std_out = CreatePipe
    }
  hPutStr hIn $ makeImageFile scene
  hClose hIn
  outp <- hGetContents hOut
  putStr outp
  hClose hOut
  exitCode <- waitForProcess handle
  putStrLn $ if exitCode /= ExitSuccess
    then "[ \x1b[31m" ++ show exitCode ++ "\x1b[0m ]"
    else "[ \x1b[32mSaved to " ++ output ++ "\x1b[0m ]"
