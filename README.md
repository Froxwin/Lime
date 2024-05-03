# Lime

<div align="center">

![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/Froxwin/Lime/haskell.yml?branch=master&style=for-the-badge&label=Haskell%20CI&logo=haskell)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/Froxwin/Lime?color=%23f5c2e7&style=for-the-badge)
![GitHub License](https://img.shields.io/github/license/Froxwin/Lime?style=for-the-badge)
![Dynamic YAML Badge](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fraw.githubusercontent.com%2FFroxwin%2FLime%2Fmaster%2F.github%2Fworkflows%2Fhaskell.yml&query=%24%5B'jobs'%5D%5B'build'%5D%5B'steps'%5D%5B1%5D%5B'with'%5D%5B'cabal-version'%5D&label=Cabal&style=for-the-badge)

</div>

Lime is a render engine written in haskell that employs recursive eye-based ray tracing.
The recursive nature of the algorithm makes it ideal to implement in a purely functional language like haskell.

## Usage

Use `cabal` to build and run the project by running the following command

```ps1
$ cabal run Lime -- [-i|--input=PATH] [-o|--output=PATH] [-p|--preview=BOOL] [-t|--textures=DIR] [-m|--models=DIR]
```

Run `cabal run Lime -- --help` for more help

For reference scenes see [examples](Examples/Scenes/)

## Features

See [TODO](TODO.md)

### Animating / Scripting

Lime supports scripting through lua to procedurally generate scenes.
For a static scene, the script should expose the global variable `scene` see
[example](Examples/Scenes/Spheres.lua).
For an animation, the script should expose the global variable `scenes` which is
a list of all frames to be rendered see [example](<Examples/Scenes/Circling camera.anim.lua>).

Running ffmpeg with the following arguments stitches the frames.

```ps1
$ ffmpeg -framerate some_fps -i "some_dir\%d.png" some_video.mp4
```

## Documentation

Run `cabal haddock` in the project directory to build the library documentation

## Example Renders

<div align="center">

![Scene](Examples/Renders/Scene.png) ![Balls](Examples/Renders/Balls.png)

![Moon](Examples/Renders/Moon.png) ![Empty](Examples/Renders/Empty.png)

![Saturn](Examples/Renders/Saturn.png) ![Saturn](Examples/Renders/Jupiter.png)

<img src="Examples/Renders/Spheres.png" width="400px"> <img src="Examples/Renders/Pawn.png" width="400px">

![Circling Camera](<Examples/Renders/Circling camera.gif>)

</div>
