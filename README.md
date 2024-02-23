# Lime

[![build](https://github.com/Froxwin/Lime/actions/workflows/haskell.yml/badge.svg)](https://github.com/Froxwin/Lime/actions/workflows/haskell.yml)

Silly Raytracer

## Usage

Scene reference
```yaml
samples: 9   # Number of samples for a single pixel, set to 1 to turn off anti-aliasing
height: 270  # Image Height
width: 480   # Image Width
camera:
  origin: [0, 150, 0]  # Camera origin vector
  looking: [0, 0, 0]   # Point camera is looking at
  fov: 1               # Focal distance
  vW: 3.5              # Width of viewport
```


