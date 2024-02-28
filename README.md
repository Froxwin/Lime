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

### Animating

With a small shell command Lime can be made to animate.

An example script is given, see [script](Scripts/Stitch.ps1).
Any parameter can be varied in the same way.
Running ffmpeg with the following arguments would generate the video

```ps1
ffmpeg.exe -framerate some_fps -i "some_dir\%d.png" some_video.mp4
```

## Documentation

Run `cabal haddock` in the project directory to build the documentation

## Example Render

| Example Scene | Example Animation |
| ------------- | ----------------- |
| ![Example Scene](Examples/Scene.png) | ![Example Animation](Examples/Scene.gif) |
