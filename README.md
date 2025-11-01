# Lime

<div align="center">
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/Froxwin/Lime/haskell.yml?branch=master&style=for-the-badge&label=Build&logo=haskell)
![GitHub License](https://img.shields.io/github/license/Froxwin/Lime?style=for-the-badge)
</div>

Lime is a small Haskell render engine that uses recursive path tracing to produce
images.
It supports scene descriptions in both YAML and Lua; Lua allows procedural scene
generation and animation.

## Build

### With cabal
``` sh
cabal build
```
### With Nix
``` sh
nix build github:Froxwin/Lime
```

## Usage

```sh
Lime [-i|--input PATH] [-o|--output PATH] [-p|--preview] [-t|--textures DIR] [-m|--models DIR]
```

Run `Lime --help` for full options.

Example scenes can be found in [examples](Examples/Scenes/).

### Lua

For a static scene, define a global `scene`. See [example](Examples/Scenes/Spheres.lua).

For an animation, define a global `scenes` list (one entry per frame). See [example](<Examples/Scenes/Circling camera.anim.lua>).

To stitch rendered frames into a video:
``` sh
ffmpeg -framerate <fps> -i "<directory>/%d.png" output.mp4
```

## Example Renders

<div align="center">

![Scene](Examples/Renders/Scene.png) ![Balls](Examples/Renders/Balls.png)

![Moon](Examples/Renders/Moon.png) ![Empty](Examples/Renders/Empty.png)

![Saturn](Examples/Renders/Saturn.png) ![Saturn](Examples/Renders/Jupiter.png)

<img src="Examples/Renders/Spheres.png" width="400px"> <img src="Examples/Renders/Pawn.png" width="400px">

![Circling Camera](<Examples/Renders/Circling camera.gif>)

</div>
