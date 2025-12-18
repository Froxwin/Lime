#include "data.cuh"
#include "math.cuh"

extern "C" __global__ void add_kernel(const float *a, const float *b,
                                      float *out, int n) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n) {
    out[i] = a[i] + b[i];
  }
}

extern "C" __global__ void render(uchar3 *pixels, const Scene scene) {
  int x = blockIdx.x * blockDim.x + threadIdx.x;
  int y = blockIdx.y * blockDim.y + threadIdx.y;

  if (x >= scene.width || y >= scene.height)
    return;

  float ui = float(x) / float(scene.width - 1);
  float vi = float(y) / float(scene.height - 1);

  // camera
  float3 w = normalize(scene.camera.position - scene.camera.looking_at);
  float3 u = normalize(cross(scene.camera.up_vector, w));
  float3 v = cross(w, u);

  // aperture
  float disk_radius = scene.camera.focal_length *
                      tan(scene.camera.field_of_view / 2) *
                      scene.camera.focal_length;
  float3 disk_u = disk_radius * u;
  float3 disk_v = disk_radius * v;

  // viewport
  float viewport_height = 2 * tan(scene.camera.field_of_view / 2) * scene.camera.focal_length;
  float viewport_width = viewport_height * ((float)scene.width / (float)scene.height);
  float3 horizontal = viewport_width * u;
  float3 vertical = viewport_height * v;
  float3 bottom_left = scene.camera.position - (horizontal / 2) - (vertical / 2) - (scene.camera.focal_length * w);

  unsigned char r = (unsigned char)(ui * 255.0f);
  unsigned char g = (unsigned char)(vi * 255.0f);
  unsigned char b = (unsigned char)((1.0f - ui) * 255.0f);

  pixels[y * scene.width + x] = make_uchar3(r, g, b);
}
