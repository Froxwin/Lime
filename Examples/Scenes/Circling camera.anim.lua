scenes = {}
frames = 48

for theta = 0, 2 * math.pi, 2 * math.pi / frames do
    table.insert(scenes, {
        height = 255,
        width = 400,
        samples_per_pixel = 200,
        maximum_bounces = 50,
        textures = {{"env", "env2.jpg"}},
        world = {{
            tag = "sphere",
            center = {0, 0.7, 0.5},
            radius = 1,
            material = {
                tag = "metal",
                fuzz = 0,
                texture = {
                    tag = "solid-color",
                    color = {255, 143, 107}
                }
            }
        }},
        camera = {
            position = {3 * math.cos(theta), 2, 3 * math.sin(theta) + 0.5},
            looking_at = {0, 0.7, 0.5},
            focal_length = 1,
            field_of_view = 1.57,
            upward_vector = {0, 1, 0},
            defocus_angle = 0,
            background_texture = {
                tag = "image-texture",
                image = "env"
            }
        }
    })
end
