extern "C" __global__
void add_kernel(const float* a, const float* b, float* out, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        out[i] = a[i] + b[i];
    }
}

extern "C" __global__
void render(uchar3* pixels, int width, int height) {
    // Find our pixel coordinates
    int x = blockIdx.x * blockDim.x + threadIdx.x;
    int y = blockIdx.y * blockDim.y + threadIdx.y;

    // Stay within canvas
    if (x >= width || y >= height) return;

    // Normalize to [0, 1]
    float u = float(x) / float(width - 1);
    float v = float(y) / float(height - 1);

    // Create a gradient – here, horizon fades from black to deep violet
    unsigned char r = (unsigned char)(u * 255.0f);
    unsigned char g = (unsigned char)(v * 255.0f);
    unsigned char b = (unsigned char)((1.0f - u) * 255.0f);

    pixels[y * width + x] = make_uchar3(r, g, b);
}
