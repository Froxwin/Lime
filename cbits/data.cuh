#ifndef DATA_CUH
#define DATA_CUH

struct Camera {
  float3 position;
  float3 looking_at;
  float focal_length;
  float field_of_view;
  float3 up_vector;
  float defocus_angle;
};

struct Scene {
  int width;
  int height;
  int samples_per_pixel;
  int max_bounces;
  double exposure;
  Camera camera;
};

#endif
