# Lime

![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/Froxwin/Lime/haskell.yml?branch=master&style=for-the-badge&label=Haskell%20CI&logo=haskell)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/Froxwin/Lime?color=%23f5c2e7&style=for-the-badge)
![GitHub License](https://img.shields.io/github/license/Froxwin/Lime?style=for-the-badge)
![Dynamic YAML Badge](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fraw.githubusercontent.com%2FFroxwin%2FLime%2Fmaster%2F.github%2Fworkflows%2Fhaskell.yml&query=%24%5B'jobs'%5D%5B'build'%5D%5B'steps'%5D%5B1%5D%5B'with'%5D%5B'cabal-version'%5D&label=Cabal&style=for-the-badge)

Silly Raytracer

## Usage

> [!IMPORTANT]
> ffmpeg is required in the system path

Scene reference
```yaml
samples: 50   # Number of samples for a single pixel, set to 1 to turn off anti-aliasing
height:  270  # Image Height
width:   480  # Image Width
camera:
  cameraOrigin: [0, 1.2, -1.3]  # Camera origin vector
  lookingAt:    [5, 1, 0]       # Point camera is looking at
  focalLength:  1               # Focal distance
  fov:          1.57            # Horizontal view angle
  cameraUpVec:  [0, 1, 0]       # Vector perpendicular to camera horizon
  defocusAngle: 0.08            # Angle beyond which the camera defocuses objects
```

## Features

See [TODO](TODO.md)

With a small shell command Lime can be made to animate.

An example script to generate a moving camera. See [script](Scripts/Stitch.ps1)

```ps1
function getFileContent ($param) {
  $a = @"
samples: 50
height:  270
width:   480
camera:
  cameraOrigin: [0, 1.2, -1.3]
  lookingAt:    [$($param), 1, 0]
  focalLength:  1
  fov:          1.57
  cameraUpVec:  [0, 1, 0]
  defocusAngle: 0.08
"@
  return $a
}

for ($i = -5; $i -lt 5; $i+=0.1) {
  New-Item "some_dir\$($frame).yaml"
  $c = getFileContent($i)
  Add-Content -Path "some_dir\$($frame).yaml" -Value $c
  $frame++
}
```

Any parameter can be varied in the same way

This command would render each frame
```ps1
Get-ChildItem "some_dir" | ForEach-Object {
  path_to_lime -i "some_dir\$($_.BaseName).yaml" -o "somedir\$($_.BaseName)"
}
```

Running ffmpeg with the following arguments would generate the video

```ps1
ffmpeg.exe -framerate some_fps -i "some_dir\%d.png" some_video.mp4
```

## Documentation

Run `cabal haddock` in the project directory to build the documentation

## Example Render

![Example Scene](Examples/Scene.png)

<video controls src="Examples/Scene.mp4" title="Example Animation"></video>
