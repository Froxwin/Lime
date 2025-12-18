#ifndef MATH_CUH
#define MATH_CUH

__host__ __device__ inline float3 operator-(const float3 &a) {
  return make_float3(-a.x, -a.y, -a.z);
}

__host__ __device__ inline float3 operator-(const float3 &a, const float3 &b) {
  return make_float3(a.x - b.x, a.y - b.y, a.z - b.z);
}

__host__ __device__ inline float3 operator+(const float3 &a, const float3 &b) {
  return make_float3(a.x + b.x, a.y + b.y, a.z + b.z);
}

__host__ __device__ inline float3 operator*(float t, const float3 &a) {
  return make_float3(t * a.x, t * a.y, t * a.z);
}

__host__ __device__ inline float3 operator*(const float3 &a, float t) {
  return make_float3(t * a.x, t * a.y, t * a.z);
}

__host__ __device__ inline float3 operator*(const float3 &a, const float3 &b) {
  return make_float3(b.x * a.x, b.y * a.y, b.z * a.z);
}

__host__ __device__ inline float3 operator/(const float3 &a, float t) {
  return make_float3(a.x / t, a.y / t, a.z / t);
}

__host__ __device__ inline float dot(const float3 &a, const float3 &b) {
  return a.x * b.x + a.y * b.y + a.z * b.z;
}

__host__ __device__ inline float length(const float3 &v) {
  return sqrtf(dot(v, v));
}

__host__ __device__ inline float3 normalize(const float3 &v) {
  float invLen = 1.0f / length(v);
  return invLen * v;
}

__host__ __device__ inline float3 sqrtf(const float3 &color) {
  return make_float3(sqrtf(color.x), sqrtf(color.y), sqrtf(color.z));
}

__host__ __device__ inline float3 cross(const float3 &a, const float3 &b) {
  return make_float3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z,
                     a.x * b.y - a.y * b.x);
}

#endif
