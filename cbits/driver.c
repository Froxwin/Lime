#include <cuda.h>
#include <stdio.h>

extern int print_gpu_info() {
  cuInit(0);

  int driverVersion = 0;
  cuDriverGetVersion(&driverVersion);

  printf("CUDA Driver Version: %d.%d\n", driverVersion / 1000,
         (driverVersion % 1000) / 10);

  int count = 0;
  cuDeviceGetCount(&count);
  if (count == 0) {
    printf("No CUDA devices found.\n");
    return -1;
  }

  for (int i = 0; i < count; i++) {
    CUdevice dev;
    cuDeviceGet(&dev, i);

    char name[256];
    cuDeviceGetName(name, sizeof(name), dev);

    int major, minor;
    cuDeviceComputeCapability(&major, &minor, dev);

    size_t mem;
    cuDeviceTotalMem(&mem, dev);

    int maxThreadsPerBlock;
    cuDeviceGetAttribute(&maxThreadsPerBlock,
                         CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK, dev);

    int coreClock;
    cuDeviceGetAttribute(&coreClock, CU_DEVICE_ATTRIBUTE_CLOCK_RATE,
                         dev); // kHz

    int memClock;
    cuDeviceGetAttribute(&memClock, CU_DEVICE_ATTRIBUTE_MEMORY_CLOCK_RATE,
                         dev); // kHz

    int busWidth;
    cuDeviceGetAttribute(&busWidth, CU_DEVICE_ATTRIBUTE_GLOBAL_MEMORY_BUS_WIDTH,
                         dev); // bits

    printf("=== GPU %d =============================================\n", i);
    printf("Name:                %s\n", name);
    printf("Compute Capability:  %d.%d\n", major, minor);
    printf("Total Memory:        %zu MB\n", mem / (1024 * 1024));
    printf("Max Threads/Block:   %d\n", maxThreadsPerBlock);
    printf("Core Clock:          %.2f MHz\n", coreClock / 1000.0);
    printf("Memory Clock:        %.2f MHz\n", memClock / 1000.0);
    printf("Memory Bus Width:    %d-bit\n", busWidth);
    printf("========================================================\n\n");
  }

  return 0;
}

extern int launch_render(const char *ptx, unsigned char *hostPixels, int width,
                         int height) {
  CUdevice device;
  CUcontext context;
  CUmodule module;
  CUfunction kernel;

  size_t totalBytes = (size_t)width * (size_t)height * 3;

  cuInit(0);
  cuDeviceGet(&device, 0);
  cuCtxCreate(&context, 0, device);

  CUresult r = cuModuleLoadData(&module, ptx);
  if (r != CUDA_SUCCESS)
    return -1;

  r = cuModuleGetFunction(&kernel, module, "render");
  if (r != CUDA_SUCCESS)
    return -2;

  CUdeviceptr dPixels;
  cuMemAlloc(&dPixels, totalBytes);

  void *args[] = {&dPixels, &width, &height};

  int blockX = 16;
  int blockY = 16;
  int gridX = (width + blockX - 1) / blockX;
  int gridY = (height + blockY - 1) / blockY;

  cuLaunchKernel(kernel, gridX, gridY, 1, blockX, blockY, 1, 0, 0, args, 0);

  cuCtxSynchronize();

  cuMemcpyDtoH(hostPixels, dPixels, totalBytes);

  cuMemFree(dPixels);
  cuModuleUnload(module);
  cuCtxDestroy(context);

  return 0;
}
