scene = {
  height = 9 * 30,
  width = 16 * 30,
  samples_per_pixel = 100,
  maximum_bounces = 50,
  textures = {{"env", "env2.jpg"}},
  camera = {
    position = {0, 2, -2},
    looking_at = {0, 0.7, 0.5},
    focal_length = 1,
    field_of_view = 1.57,
    upward_vector = {0, 1, 0},
    defocus_angle = 0,
    background_texture = {
      tag = "image-texture",
      image = "env",
      style = {
        tag = "stretch"
      }
    }
  },
  world = {{
    tag = "sphere",
    transform = {
      {
        tag = "translate",
        contents = {0, 0.7, 0.5}
      }
    },
    material = {
      tag = "metal",
      fuzz = 0,
      texture = {
        tag = "solid-color",
        color = {255, 143, 107}
      }
    }
  }}
}

for i = 0, 100 do
  materalRoll = math.random(3)
  mat = {
    texture = {
      tag = "solid-color",
      color = {math.random(255), math.random(255), math.random(255)}
    }
  }
  if materalRoll == 1 then
    mat["tag"] = "lambertian"
  elseif materalRoll == 2 then
    mat["tag"] = "metal"
    mat["fuzz"] = math.random() * 0.5
  elseif materalRoll == 3 then
    mat["tag"] = "emissive"
  end

  table.insert(scene.world, {
    tag = "sphere",
    transform = {
      {
        tag = "translate",
        contents = {math.random(-10, 10), math.random(-1, 2), math.random(-10, 10)}
      },
      {
        tag = "scale",
        contents = {0.3 + (math.random() * 0.2), 0.3 + (math.random() * 0.2), 0.3 + (math.random() * 0.2)}
      }
    },
    material = mat
  })
end
