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

### With nix

The [nix expression](shell.nix) provides support for nix. Just run the following command to build

```sh
$ nix-shell --command build
```

### Without nix

Use `cabal` to build and run the project by running the following command

```ps1
$ cabal run Lime -- [-i|--input=PATH] [-o|--output=PATH] [-f|--force=BOOL]
```

Run `cabal run Lime -- --help` for more help

For reference scenes see [examples](Examples)

## Features

See [TODO](TODO.md)

### Animating

With a small shell command Lime can be made to animate.
For example see [script](Scripts/Stitch.ps1).
You can vary any parameter in the same way.
Running ffmpeg with the following arguments generates the video.

```ps1
$ ffmpeg -framerate some_fps -i "some_dir\%d.png" some_video.mp4
```

See [example](#example-renders)

## Documentation

Run `cabal haddock` in the project directory to build the documentation

## Example Renders

<div align="center">

![Scene](Examples/Scene.png) ![Glass](Examples/Glass.png)

![Moon](Examples/Moon.png) ![Balls](Examples/Balls.png)

</div>
