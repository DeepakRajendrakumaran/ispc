/*
  Copyright Intel Corporation
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    * Neither the name of Intel Corporation nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.


   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
   TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifdef _MSC_VER
#define NOMINMAX
#pragma warning(disable : 4244)
#pragma warning(disable : 4305)
// preventing MSVC fopen() deprecation complaints
#define _CRT_SECURE_NO_DEPRECATE
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __linux__
#include <malloc.h>
#endif
#include "L0_helpers.h"
#include "timing.h"
#include <algorithm>
#include <chrono>
#include <fstream>
#include <iostream>
#include <map>
#include <math.h>
#include <string>
#include <sys/types.h>

// ispcrt
#include "ispcrt.h"

#define NSUBSAMPLES 2
#define CORRECTNESS_THRESHOLD 0.01

using namespace hostutil;

extern void ao_serial(int w, int h, int nsubsamples, float image[]);

struct Parameters {
    int width;
    int height;
    int nsubsamples{NSUBSAMPLES};
    float *image;
};

static unsigned int niterations;
static unsigned int width, height;
static unsigned char *img;
static float *fimg;

static unsigned char clamp(float f) {
    int i = (int)(f * 255.5);

    if (i < 0)
        i = 0;
    if (i > 255)
        i = 255;

    return (unsigned char)i;
}

static void savePPM(const char *fname, int w, int h, float *fimg) {
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            img[3 * (y * w + x) + 0] = clamp(fimg[3 * (y * w + x) + 0]);
            img[3 * (y * w + x) + 1] = clamp(fimg[3 * (y * w + x) + 1]);
            img[3 * (y * w + x) + 2] = clamp(fimg[3 * (y * w + x) + 2]);
        }
    }

    FILE *fp = fopen(fname, "wb");
    if (!fp) {
        perror(fname);
        exit(1);
    }

    fprintf(fp, "P6\n");
    fprintf(fp, "%d %d\n", w, h);
    fprintf(fp, "255\n");
    fwrite(img, w * h * 3, 1, fp);
    fclose(fp);
    printf("Wrote image file %s\n", fname);
}

static int run() {
    std::cout.setf(std::ios::unitbuf);
    ispcrtSetErrorFunc([](ISPCRTError e, const char *m) {
        std::cerr << "ISPCRT Error! --> " << m << std::endl;
        std::exit(1);
    });

    size_t imgSize = width * height * 3;
    img = new unsigned char[imgSize];
    fimg = (float *)aligned_alloc(4096, imgSize * sizeof(float));

    uint64_t minKernelClocksGPU = std::numeric_limits<uint64_t>::max();

    // Init compute device (CPU or GPU)
    auto run_kernel = [&](ISPCRTDeviceType type) {
        auto device = ispcrtGetDevice(type);

        // Setup output array
        auto buf_dev = ispcrtNewMemoryView(device, fimg, imgSize * sizeof(float));

        // Setup parameters structure
        Parameters p;
        p.width = width;
        p.height = height;
        p.image = (float *)ispcrtDevicePtr(buf_dev);

        auto p_dev = ispcrtNewMemoryView(device, &p, sizeof(p));

        // Create module and kernel to execute
        auto module = ispcrtLoadModule(device, "genx_aobench");
        auto kernel = ispcrtNewKernel(device, module, "ao_ispc");
        // Create task queue and execute kernel
        auto queue = ispcrtNewTaskQueue(device);

        // Run the ispc gpu code nIterations times, and report the minimum
        // time for any of them.
        const char *device_str = (type == ISPCRT_DEVICE_TYPE_GPU) ? "GPU" : "CPU";
        double minCyclesISPC = 1e30;
        double kernelTicks = 1e30;
        for (unsigned int i = 0; i < niterations; i++) {
            memset((void *)fimg, 0, sizeof(float) * width * height * 3);
            ispcrtCopyToDevice(queue, p_dev);
            ispcrtDeviceBarrier(queue);
            ispcrtSync(queue);
            reset_and_start_timer();
            auto res = ispcrtLaunch2D(queue, kernel, p_dev, height * width / 16, 1);
            ispcrtRetain(res);
            ispcrtDeviceBarrier(queue);
            ispcrtSync(queue);

            if (ispcrtFutureIsValid(res)) {
                kernelTicks = ispcrtFutureGetTimeNs(res) * 1e-6;
            }
            ispcrtRelease(res);
            double mcycles = get_elapsed_mcycles();
            printf("@time of %s run:\t\t\t[%.3f] milliseconds\n", device_str, kernelTicks);
            printf("@time of %s run:\t\t\t[%.3f] million cycles\n", device_str, mcycles);
            minCyclesISPC = std::min(minCyclesISPC, mcycles);
            ispcrtCopyToHost(queue, buf_dev);
            ispcrtSync(queue);
        }

        printf("[aobench ISPC GPU]:\t\t[%.3f] million cycles (%d x %d image)\n", minCyclesISPC, width, height);
    };
    run_kernel(ISPCRT_DEVICE_TYPE_CPU);
    savePPM("ao-ispc-cpu.ppm", width, height, fimg);

    run_kernel(ISPCRT_DEVICE_TYPE_GPU);
    savePPM("ao-ispc-gpu.ppm", width, height, fimg);

    // Run the serial code nIterations times, and report the minimum
    // time for any of them.
    double minCyclesSerial = 1e30;
    for (unsigned int i = 0; i < niterations; i++) {
        memset((void *)fimg, 0, sizeof(float) * width * height * 3);
        reset_and_start_timer();
        auto wct = std::chrono::system_clock::now();
        ao_serial(width, height, NSUBSAMPLES, fimg);
        double mcycles = get_elapsed_mcycles();
        auto dur = (std::chrono::system_clock::now() - wct);
        auto secs = std::chrono::duration_cast<std::chrono::milliseconds>(dur);
        printf("@time of CPU run:\t\t\t[%ld] milliseconds\n", secs.count());
        printf("@time of CPU run:\t\t\t[%.3f] million cycles\n", mcycles);
        minCyclesSerial = std::min(minCyclesSerial, mcycles);
    }
    savePPM("ao-cpp-serial.ppm", width, height, fimg);
    printf("[aobench serial]:\t\t[%.3f] million cycles (%d x %d image)\n", minCyclesSerial, width, height);
    delete[] img;
    free(fimg);
    return 0;
}

int main(int argc, char *argv[]) {
    if (argc != 4) {
        printf("%s\n", argv[0]);
        printf("Usage: ao [num test iterations] [width] [height]\n");
        getchar();
        exit(-1);
    } else {
        niterations = atoi(argv[1]);
        width = atoi(argv[2]);
        height = atoi(argv[3]);
    }

    int success = 0;

    std::cout << "Running test with " << niterations << " iterations of ISPC on GEN and CPU on " << width << " * "
              << height << " size." << std::endl;
    success = run();

    return success;
}
