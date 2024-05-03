scene = {
  height = 9 * 30,
  width = 16 * 30,
  samples_per_pixel = 200,
  maximum_bounces = 50,
  textures = {{"env", "env2.jpg"}},
  objects = {{"pawn", "pawn.obj"}},
  camera = {
    position = {0, 4, -4},
    looking_at = {0, 2, 0},
    focal_length = 1,
    field_of_view = 1.57,
    upward_vector = {0, 1, 0},
    defocus_angle = 0,
    background_texture = {
      tag = "image-texture",
      image = "env"
    }
  },
  world = {{
    tag = "plane",
    point = {0, -1, 0},
    normal = {0, 1, 0},
    material = {
      tag = "emissive",
      texture = {
        tag = "checkered",
        color1 = {255, 255, 255},
        color2 = {0, 0, 0},
        scale = 5
      }
    }
  }, {
    tag = "mesh",
    obj_file = "pawn",
    material = {
      tag = "metal",
      fuzz = 0.2,
      texture = {
        tag = "solid-color",
        color = {255, 205, 102}
      }
    }
  }}
}
