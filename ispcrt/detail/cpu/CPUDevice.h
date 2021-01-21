// Copyright Intel Corporation
// SPDX-License-Identifier: BSD-3-Clause

#pragma once

#include "../Device.h"
#include "../Future.h"

namespace ispcrt {

struct CPUDevice : public base::Device {
    CPUDevice() = default;

    base::MemoryView *newMemoryView(void *appMem, size_t numBytes) const override;

    base::TaskQueue *newTaskQueue() const override;

    base::Module *newModule(const char *moduleFile) const override;

    base::Kernel *newKernel(const base::Module &module, const char *name) const override;
};

} // namespace ispcrt
