/*
  Copyright (c) 2010-2020, Intel Corporation
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

/** @file ispc.cpp
    @brief ispc global definitions
*/

#include "ispc.h"
#include "llvmutil.h"
#include "module.h"
#include "util.h"

#include <sstream>
#include <stdarg.h> /* va_list, va_start, va_arg, va_end */
#include <stdio.h>
#ifdef ISPC_HOST_IS_WINDOWS
#include <direct.h>
#include <windows.h>
#define strcasecmp stricmp
#include <intrin.h>
#else // !ISPC_HOST_IS_WINDOWS
#include <sys/types.h>
#include <unistd.h>
#endif // ISPC_HOST_IS_WINDOWS

#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/CodeGen/TargetLowering.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/ADT/SmallBitVector.h>

Globals *g;
Module *m;

///////////////////////////////////////////////////////////////////////////
// Target

#if defined(__arm__) || defined(__aarch64__)
#define ARM_HOST
#endif

#if !defined(ISPC_HOST_IS_WINDOWS) && !defined(ARM_HOST)
// __cpuid() and __cpuidex() are defined on Windows in <intrin.h> for x86/x64.
// On *nix they need to be defined manually through inline assembler.
static void __cpuid(int info[4], int infoType) {
    __asm__ __volatile__("cpuid" : "=a"(info[0]), "=b"(info[1]), "=c"(info[2]), "=d"(info[3]) : "0"(infoType));
}

/* Save %ebx in case it's the PIC register */
static void __cpuidex(int info[4], int level, int count) {
    __asm__ __volatile__("xchg{l}\t{%%}ebx, %1\n\t"
                         "cpuid\n\t"
                         "xchg{l}\t{%%}ebx, %1\n\t"
                         : "=a"(info[0]), "=r"(info[1]), "=c"(info[2]), "=d"(info[3])
                         : "0"(level), "2"(count));
}
#endif // !ISPC_HOST_IS_WINDOWS && !__ARM__ && !__AARCH64__

#ifndef ARM_HOST
static bool __os_has_avx_support() {
#if defined(ISPC_HOST_IS_WINDOWS)
    // Check if the OS will save the YMM registers
    unsigned long long xcrFeatureMask = _xgetbv(_XCR_XFEATURE_ENABLED_MASK);
    return (xcrFeatureMask & 6) == 6;
#else  // !defined(ISPC_HOST_IS_WINDOWS)
    // Check xgetbv; this uses a .byte sequence instead of the instruction
    // directly because older assemblers do not include support for xgetbv and
    // there is no easy way to conditionally compile based on the assembler used.
    int rEAX, rEDX;
    __asm__ __volatile__(".byte 0x0f, 0x01, 0xd0" : "=a"(rEAX), "=d"(rEDX) : "c"(0));
    return (rEAX & 6) == 6;
#endif // !defined(ISPC_HOST_IS_WINDOWS)
}

static bool __os_has_avx512_support() {
#if defined(ISPC_HOST_IS_WINDOWS)
    // Check if the OS saves the XMM, YMM and ZMM registers, i.e. it supports AVX2 and AVX512.
    // See section 2.1 of software.intel.com/sites/default/files/managed/0d/53/319433-022.pdf
    unsigned long long xcrFeatureMask = _xgetbv(_XCR_XFEATURE_ENABLED_MASK);
    return (xcrFeatureMask & 0xE6) == 0xE6;
#else  // !defined(ISPC_HOST_IS_WINDOWS)
    // Check xgetbv; this uses a .byte sequence instead of the instruction
    // directly because older assemblers do not include support for xgetbv and
    // there is no easy way to conditionally compile based on the assembler used.
    int rEAX, rEDX;
    __asm__ __volatile__(".byte 0x0f, 0x01, 0xd0" : "=a"(rEAX), "=d"(rEDX) : "c"(0));
    return (rEAX & 0xE6) == 0xE6;
#endif // !defined(ISPC_HOST_IS_WINDOWS)
}
#endif // !ARM_HOST

static ISPCTarget lGetSystemISA() {
#ifdef ARM_HOST
    return ISPCTarget::neon_i32x4;
#else
    int info[4];
    __cpuid(info, 1);

    int info2[4];
    // Call cpuid with eax=7, ecx=0
    __cpuidex(info2, 7, 0);

    if ((info[2] & (1 << 27)) != 0 &&  // OSXSAVE
        (info2[1] & (1 << 5)) != 0 &&  // AVX2
        (info2[1] & (1 << 16)) != 0 && // AVX512 F
        __os_has_avx512_support()) {
        // We need to verify that AVX2 is also available,
        // as well as AVX512, because our targets are supposed
        // to use both.

        if ((info2[1] & (1 << 17)) != 0 && // AVX512 DQ
            (info2[1] & (1 << 28)) != 0 && // AVX512 CDI
            (info2[1] & (1 << 30)) != 0 && // AVX512 BW
            (info2[1] & (1 << 31)) != 0) { // AVX512 VL
            return ISPCTarget::avx512skx_i32x16;
        } else if ((info2[1] & (1 << 26)) != 0 && // AVX512 PF
                   (info2[1] & (1 << 27)) != 0 && // AVX512 ER
                   (info2[1] & (1 << 28)) != 0) { // AVX512 CDI
            return ISPCTarget::avx512knl_i32x16;
        }
        // If it's unknown AVX512 target, fall through and use AVX2
        // or whatever is available in the machine.
    }

    if ((info[2] & (1 << 27)) != 0 &&                           // OSXSAVE
        (info[2] & (1 << 28)) != 0 && __os_has_avx_support()) { // AVX
        // AVX1 for sure....
        // Ivy Bridge?
        if ((info[2] & (1 << 29)) != 0 && // F16C
            (info[2] & (1 << 30)) != 0 && // RDRAND
            (info2[1] & (1 << 5)) != 0) { // AVX2.
            return ISPCTarget::avx2_i32x8;
        }
        // Regular AVX
        return ISPCTarget::avx1_i32x8;
    } else if ((info[2] & (1 << 19)) != 0)
        return ISPCTarget::sse4_i32x4;
    else if ((info[3] & (1 << 26)) != 0)
        return ISPCTarget::sse2_i32x4;
    else {
        Error(SourcePos(), "Unable to detect supported SSE/AVX ISA.  Exiting.");
        exit(1);
    }
#endif
}

static const bool lIsTargetValidforArch(ISPCTarget target, Arch arch) {
    bool ret = true;
    // If target name starts with sse or avx, has to be x86 or x86-64.
    if (ISPCTargetIsX86(target)) {
        if (arch != Arch::x86_64 && arch != Arch::x86)
            ret = false;
    } else if (target == ISPCTarget::neon_i8x16 || target == ISPCTarget::neon_i16x8) {
        if (arch != Arch::arm)
            ret = false;
    } else if (target == ISPCTarget::neon_i32x4 || target == ISPCTarget::neon_i32x8) {
        if (arch != Arch::arm && arch != Arch::aarch64)
            ret = false;
    }

    return ret;
}

typedef enum {
    // Special value, indicates that no CPU is present.
    CPU_None = 0,

    // A generic 64-bit specific x86 processor model which tries to be good
    // for modern chips without enabling instruction set encodings past the
    // basic SSE2 and 64-bit ones
    CPU_x86_64 = 1,

    // Early Atom CPU. Supports SSSE3.
    CPU_Bonnell,

    // Generic Core2-like. Supports SSSE3. Isn`t quite compatible with Bonnell,
    // but for ISPC the difference is negligible; ISPC doesn`t make use of it.
    CPU_Core2,

    // Core2 Solo/Duo/Quad/Extreme. Supports SSE 4.1 (but not 4.2).
    CPU_Penryn,

    // Late Core2-like. Supports SSE 4.2 + POPCNT/LZCNT.
    CPU_Nehalem,

    // CPU in PS4/Xbox One.
    CPU_PS4,

    // Sandy Bridge. Supports AVX 1.
    CPU_SandyBridge,

    // Ivy Bridge. Supports AVX 1 + RDRAND.
    CPU_IvyBridge,

    // Haswell. Supports AVX 2.
    CPU_Haswell,

    // Broadwell. Supports AVX 2 + ADX/RDSEED/SMAP.
    CPU_Broadwell,

    // Knights Landing - Xeon Phi.
    // Supports AVX-512F: All the key AVX-512 features: masking, broadcast... ;
    //          AVX-512CDI: Conflict Detection;
    //          AVX-512ERI & PRI: 28-bit precision RCP, RSQRT and EXP transcendentals,
    //                            new prefetch instructions.
    CPU_KNL,
    // Skylake Xeon.
    // Supports AVX-512F: All the key AVX-512 features: masking, broadcast... ;
    //          AVX-512CDI: Conflict Detection;
    //          AVX-512VL: Vector Length Orthogonality;
    //          AVX-512DQ: New HPC ISA (vs AVX512F);
    //          AVX-512BW: Byte and Word Support.
    CPU_SKX,

    // Icelake client
    CPU_ICL,

    // Late Atom-like design. Supports SSE 4.2 + POPCNT/LZCNT.
    CPU_Silvermont,

// FIXME: LLVM supports a ton of different ARM CPU variants--not just
// cortex-a9 and a15.  We should be able to handle any of them that also
// have NEON support.
#ifdef ISPC_ARM_ENABLED
    // ARM Cortex A9. Supports NEON VFPv3.
    CPU_CortexA9,

    // ARM Cortex A15. Supports NEON VFPv4.
    CPU_CortexA15,

    // ARM Cortex A35, A53, A57.
    CPU_CortexA35,
    CPU_CortexA53,
    CPU_CortexA57,
#endif

    sizeofCPUtype
} CPUtype;

class AllCPUs {
  private:
    std::vector<std::vector<std::string>> names;
    std::vector<std::set<CPUtype>> compat;

    std::set<CPUtype> Set(int type, ...) {
        std::set<CPUtype> retn;
        va_list args;

        retn.insert((CPUtype)type);
        va_start(args, type);
        while ((type = va_arg(args, int)) != CPU_None)
            retn.insert((CPUtype)type);
        va_end(args);

        return retn;
    }

  public:
    AllCPUs() {
        names = std::vector<std::vector<std::string>>(sizeofCPUtype);
        compat = std::vector<std::set<CPUtype>>(sizeofCPUtype);

        names[CPU_None].push_back("");

        names[CPU_x86_64].push_back("x86-64");

        names[CPU_Bonnell].push_back("atom");
        names[CPU_Bonnell].push_back("bonnell");

        names[CPU_Core2].push_back("core2");

        names[CPU_Penryn].push_back("penryn");

        names[CPU_Silvermont].push_back("slm");
        names[CPU_Silvermont].push_back("silvermont");

        names[CPU_Nehalem].push_back("corei7");
        names[CPU_Nehalem].push_back("nehalem");

        names[CPU_PS4].push_back("btver2");
        names[CPU_PS4].push_back("ps4");

        names[CPU_SandyBridge].push_back("corei7-avx");
        names[CPU_SandyBridge].push_back("sandybridge");

        names[CPU_IvyBridge].push_back("core-avx-i");
        names[CPU_IvyBridge].push_back("ivybridge");

        names[CPU_Haswell].push_back("core-avx2");
        names[CPU_Haswell].push_back("haswell");

        names[CPU_Broadwell].push_back("broadwell");

        names[CPU_KNL].push_back("knl");

        names[CPU_SKX].push_back("skx");

        names[CPU_ICL].push_back("icelake-client");
        names[CPU_ICL].push_back("icl");

#ifdef ISPC_ARM_ENABLED
        names[CPU_CortexA9].push_back("cortex-a9");

        names[CPU_CortexA15].push_back("cortex-a15");

        names[CPU_CortexA35].push_back("cortex-a35");

        names[CPU_CortexA53].push_back("cortex-a53");

        names[CPU_CortexA57].push_back("cortex-a57");
#endif

        Assert(names.size() == sizeofCPUtype);

        compat[CPU_Silvermont] =
            Set(CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont, CPU_None);

        compat[CPU_KNL] = Set(CPU_KNL, CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont,
                              CPU_SandyBridge, CPU_IvyBridge, CPU_Haswell, CPU_Broadwell, CPU_None);

        compat[CPU_SKX] = Set(CPU_SKX, CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont,
                              CPU_SandyBridge, CPU_IvyBridge, CPU_Haswell, CPU_Broadwell, CPU_None);

        compat[CPU_ICL] = Set(CPU_ICL, CPU_SKX, CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem,
                              CPU_Silvermont, CPU_SandyBridge, CPU_IvyBridge, CPU_Haswell, CPU_Broadwell, CPU_None);

        compat[CPU_Broadwell] = Set(CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont,
                                    CPU_SandyBridge, CPU_IvyBridge, CPU_Haswell, CPU_Broadwell, CPU_None);
        compat[CPU_Haswell] = Set(CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont,
                                  CPU_SandyBridge, CPU_IvyBridge, CPU_Haswell, CPU_Broadwell, CPU_None);
        compat[CPU_IvyBridge] = Set(CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont,
                                    CPU_SandyBridge, CPU_IvyBridge, CPU_None);
        compat[CPU_SandyBridge] =
            Set(CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont, CPU_SandyBridge, CPU_None);
        compat[CPU_PS4] = Set(CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont,
                              CPU_SandyBridge, CPU_PS4, CPU_None);
        compat[CPU_Nehalem] =
            Set(CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont, CPU_None);
        compat[CPU_Penryn] = Set(CPU_x86_64, CPU_Bonnell, CPU_Penryn, CPU_Core2, CPU_Nehalem, CPU_Silvermont, CPU_None);
        compat[CPU_Core2] = Set(CPU_x86_64, CPU_Bonnell, CPU_Core2, CPU_None);
        compat[CPU_Bonnell] = Set(CPU_x86_64, CPU_Bonnell, CPU_Core2, CPU_None);

        compat[CPU_x86_64] = Set(CPU_x86_64, CPU_None);

#ifdef ISPC_ARM_ENABLED
        compat[CPU_CortexA15] = Set(CPU_CortexA9, CPU_CortexA15, CPU_None);
        compat[CPU_CortexA9] = Set(CPU_CortexA9, CPU_None);
        compat[CPU_CortexA35] = Set(CPU_CortexA35, CPU_None);
        compat[CPU_CortexA53] = Set(CPU_CortexA53, CPU_None);
        compat[CPU_CortexA57] = Set(CPU_CortexA57, CPU_None);
#endif
    }

    std::string HumanReadableListOfNames() {
        std::stringstream CPUs;
        for (int i = CPU_x86_64; i < sizeofCPUtype; i++) {
            CPUs << names[i][0];
            if (names[i].size() > 1) {
                CPUs << " (synonyms: " << names[i][1];
                for (int j = 2, je = names[i].size(); j < je; j++)
                    CPUs << ", " << names[i][j];
                CPUs << ")";
            }
            if (i < sizeofCPUtype - 1)
                CPUs << ", ";
        }
        return CPUs.str();
    }

    std::string &GetDefaultNameFromType(CPUtype type) {
        Assert((type >= CPU_None) && (type < sizeofCPUtype));
        return names[type][0];
    }

    CPUtype GetTypeFromName(std::string name) {
        CPUtype retn = CPU_None;

        for (int i = 1; (retn == CPU_None) && (i < sizeofCPUtype); i++)
            for (int j = 0, je = names[i].size(); (retn == CPU_None) && (j < je); j++)
                if (!name.compare(names[i][j]))
                    retn = (CPUtype)i;
        return retn;
    }

    bool BackwardCompatible(CPUtype what, CPUtype with) {
        Assert((what > CPU_None) && (what < sizeofCPUtype));
        Assert((with > CPU_None) && (with < sizeofCPUtype));
        return compat[what].find(with) != compat[what].end();
    }
};

Target::Target(Arch arch, const char *cpu, ISPCTarget ispc_target, bool pic, bool printTarget)
    : m_target(NULL), m_targetMachine(NULL), m_dataLayout(NULL), m_valid(false), m_ispc_target(ispc_target),
      m_isa(SSE2), m_arch(Arch::none), m_is32Bit(true), m_cpu(""), m_attributes(""), m_tf_attributes(NULL),
      m_nativeVectorWidth(-1), m_nativeVectorAlignment(-1), m_dataTypeWidth(-1), m_vectorWidth(-1), m_generatePIC(pic),
      m_maskingIsFree(false), m_maskBitCount(-1), m_hasHalf(false), m_hasRand(false), m_hasGather(false),
      m_hasScatter(false), m_hasTranscendentals(false), m_hasTrigonometry(false), m_hasRsqrtd(false), m_hasRcpd(false),
      m_hasVecPrefetch(false) {
    CPUtype CPUID = CPU_None, CPUfromISA = CPU_None;
    AllCPUs a;
    std::string featuresString;

    if (cpu) {
        CPUID = a.GetTypeFromName(cpu);
        if (CPUID == CPU_None) {
            Error(SourcePos(),
                  "Error: CPU type \"%s\" unknown. Supported"
                  " CPUs: %s.",
                  cpu, a.HumanReadableListOfNames().c_str());
            return;
        }
    }

    if (m_ispc_target == ISPCTarget::none) {
        // If a CPU was specified explicitly, try to pick the best
        // possible ISA based on that.
        switch (CPUID) {
        case CPU_None: {
            // No CPU and no ISA, so use system info to figure out
            // what this CPU supports.
            m_ispc_target = lGetSystemISA();
            std::string target_string = ISPCTargetToString(m_ispc_target);
            Warning(SourcePos(),
                    "No --target specified on command-line."
                    " Using default system target \"%s\".",
                    target_string.c_str());
            break;
        }

#ifdef ISPC_ARM_ENABLED
        case CPU_CortexA9:
        case CPU_CortexA15:
        case CPU_CortexA35:
        case CPU_CortexA53:
        case CPU_CortexA57:
            m_ispc_target = ISPCTarget::neon_i32x4;
            break;
#endif

        case CPU_KNL:
            m_ispc_target = ISPCTarget::avx512knl_i32x16;
            break;

        case CPU_ICL:
        case CPU_SKX:
            m_ispc_target = ISPCTarget::avx512skx_i32x16;
            break;

        case CPU_Broadwell:
        case CPU_Haswell:
            m_ispc_target = ISPCTarget::avx2_i32x8;
            break;

        case CPU_IvyBridge:
        case CPU_SandyBridge:
            m_ispc_target = ISPCTarget::avx1_i32x8;
            break;

        // Penryn is here because ISPC does not use SSE 4.2
        case CPU_Penryn:
        case CPU_Nehalem:
        case CPU_Silvermont:
            m_ispc_target = ISPCTarget::sse4_i32x4;
            break;

        case CPU_PS4:
            m_ispc_target = ISPCTarget::avx1_i32x4;
            break;

        default:
            m_ispc_target = ISPCTarget::sse2_i32x4;
            break;
        }
        if (CPUID != CPU_None) {
            std::string target_string = ISPCTargetToString(m_ispc_target);
            Warning(SourcePos(),
                    "No --target specified on command-line."
                    " Using ISA \"%s\" based on specified CPU \"%s\".",
                    target_string.c_str(), cpu);
        }
    }

    if (m_ispc_target == ISPCTarget::host) {
        m_ispc_target = lGetSystemISA();
    }

    if (arch == Arch::none) {
#ifdef ISPC_ARM_ENABLED
        if (ISPCTargetIsNeon(m_ispc_target)) {
#if defined(__arm__)
            arch = Arch::arm;
#else
            arch = Arch::aarch64;
#endif
        } else
#endif
            arch = Arch::x86_64;
    }

    bool error = false;

    // Make sure the target architecture is a known one; print an error
    // with the valid ones otherwise.
    for (llvm::TargetRegistry::iterator iter = llvm::TargetRegistry::targets().begin();
         iter != llvm::TargetRegistry::targets().end(); ++iter) {
        if (ArchToString(arch) == iter->getName()) {
            this->m_target = &*iter;
            break;
        }
    }
    if (this->m_target == NULL) {
        std::string error_message;
        error_message = "Invalid architecture \"";
        error_message += ArchToString(arch);
        error_message += "\"\nOptions: ";
        llvm::TargetRegistry::iterator iter;
        const char *separator = "";
        for (iter = llvm::TargetRegistry::targets().begin(); iter != llvm::TargetRegistry::targets().end(); ++iter) {
            error_message += separator;
            error_message += iter->getName();
            separator = ", ";
        }
        error_message += ".";
        Error(SourcePos(), "%s", error_message.c_str());
        error = true;
    } else {
        this->m_arch = arch;
    }

    // Ensure that we have a valid target/arch combination.
    if (!lIsTargetValidforArch(m_ispc_target, arch)) {
        std::string str_arch = ArchToString(arch);
        std::string target_string = ISPCTargetToString(m_ispc_target);
        Error(SourcePos(), "arch = %s and target = %s is not a valid combination.", str_arch.c_str(),
              target_string.c_str());
        return;
    }

    // Check default LLVM generated targets
    bool unsupported_target = false;
    switch (m_ispc_target) {
    case ISPCTarget::sse2_i32x4:
        this->m_isa = Target::SSE2;
        this->m_nativeVectorWidth = 4;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 4;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        CPUfromISA = CPU_x86_64;
        break;
    case ISPCTarget::sse2_i32x8:
        this->m_isa = Target::SSE2;
        this->m_nativeVectorWidth = 4;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 8;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        CPUfromISA = CPU_Core2;
        break;
    case ISPCTarget::sse4_i8x16:
        this->m_isa = Target::SSE4;
        this->m_nativeVectorWidth = 16;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 8;
        this->m_vectorWidth = 16;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 8;
        CPUfromISA = CPU_Nehalem;
        break;
    case ISPCTarget::sse4_i16x8:
        this->m_isa = Target::SSE4;
        this->m_nativeVectorWidth = 8;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 16;
        this->m_vectorWidth = 8;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 16;
        CPUfromISA = CPU_Nehalem;
        break;
    case ISPCTarget::sse4_i32x4:
        this->m_isa = Target::SSE4;
        this->m_nativeVectorWidth = 4;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 4;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        CPUfromISA = CPU_Nehalem;
        break;
    case ISPCTarget::sse4_i32x8:
        this->m_isa = Target::SSE4;
        this->m_nativeVectorWidth = 4;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 8;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        CPUfromISA = CPU_Nehalem;
        break;
    case ISPCTarget::avx1_i32x4:
        this->m_isa = Target::AVX;
        this->m_nativeVectorWidth = 8;
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 4;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        CPUfromISA = CPU_SandyBridge;
        break;
    case ISPCTarget::avx1_i32x8:
        this->m_isa = Target::AVX;
        this->m_nativeVectorWidth = 8;
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 8;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        CPUfromISA = CPU_SandyBridge;
        break;
    case ISPCTarget::avx1_i32x16:
        this->m_isa = Target::AVX;
        this->m_nativeVectorWidth = 8;
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 16;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        CPUfromISA = CPU_SandyBridge;
        break;
    case ISPCTarget::avx1_i64x4:
        this->m_isa = Target::AVX;
        this->m_nativeVectorWidth = 8; /* native vector width in terms of floats */
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 64;
        this->m_vectorWidth = 4;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 64;
        CPUfromISA = CPU_SandyBridge;
        break;
    case ISPCTarget::avx2_i8x32:
        this->m_isa = Target::AVX2;
        this->m_nativeVectorWidth = 32;
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 8;
        this->m_vectorWidth = 32;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 8;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = true;
        CPUfromISA = CPU_Haswell;
        break;
    case ISPCTarget::avx2_i16x16:
        this->m_isa = Target::AVX2;
        this->m_nativeVectorWidth = 16;
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 16;
        this->m_vectorWidth = 16;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 16;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = true;
        CPUfromISA = CPU_Haswell;
        break;
    case ISPCTarget::avx2_i32x4:
        this->m_isa = Target::AVX2;
        this->m_nativeVectorWidth = 8;
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 4;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = true;
        CPUfromISA = CPU_Haswell;
        break;
    case ISPCTarget::avx2_i32x8:
        this->m_isa = Target::AVX2;
        this->m_nativeVectorWidth = 8;
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 8;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = true;
        CPUfromISA = CPU_Haswell;
        break;
    case ISPCTarget::avx2_i32x16:
        this->m_isa = Target::AVX2;
        this->m_nativeVectorWidth = 16;
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 16;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = true;
        CPUfromISA = CPU_Haswell;
        break;
    case ISPCTarget::avx2_i64x4:
        this->m_isa = Target::AVX2;
        this->m_nativeVectorWidth = 8; /* native vector width in terms of floats */
        this->m_nativeVectorAlignment = 32;
        this->m_dataTypeWidth = 64;
        this->m_vectorWidth = 4;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 64;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = true;
        CPUfromISA = CPU_Haswell;
        break;
    case ISPCTarget::avx512knl_i32x16:
        this->m_isa = Target::KNL_AVX512;
        this->m_nativeVectorWidth = 16;
        this->m_nativeVectorAlignment = 64;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 16;
        this->m_maskingIsFree = true;
        this->m_maskBitCount = 1;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = this->m_hasScatter = true;
        this->m_hasTranscendentals = false;
        // For MIC it is set to true due to performance reasons. The option should be tested.
        this->m_hasTrigonometry = false;
        this->m_hasRsqrtd = this->m_hasRcpd = false;
        this->m_hasVecPrefetch = false;
        CPUfromISA = CPU_KNL;
        break;
    case ISPCTarget::avx512skx_i32x8:
        this->m_isa = Target::SKX_AVX512;
        this->m_nativeVectorWidth = 16;
        this->m_nativeVectorAlignment = 64;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 8;
        this->m_maskingIsFree = true;
        this->m_maskBitCount = 1;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = this->m_hasScatter = true;
        this->m_hasTranscendentals = false;
        this->m_hasTrigonometry = false;
        this->m_hasRsqrtd = this->m_hasRcpd = false;
        this->m_hasVecPrefetch = false;
        CPUfromISA = CPU_SKX;
        this->m_funcAttributes.push_back(std::make_pair("prefer-vector-width", "256"));
        this->m_funcAttributes.push_back(std::make_pair("min-legal-vector-width", "256"));
        break;
    case ISPCTarget::avx512skx_i32x16:
        this->m_isa = Target::SKX_AVX512;
        this->m_nativeVectorWidth = 16;
        this->m_nativeVectorAlignment = 64;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 16;
        this->m_maskingIsFree = true;
        this->m_maskBitCount = 1;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = this->m_hasScatter = true;
        this->m_hasTranscendentals = false;
        this->m_hasTrigonometry = false;
        this->m_hasRsqrtd = this->m_hasRcpd = false;
        this->m_hasVecPrefetch = false;
        CPUfromISA = CPU_SKX;
        if (g->opt.disableZMM) {
            this->m_funcAttributes.push_back(std::make_pair("prefer-vector-width", "256"));
            this->m_funcAttributes.push_back(std::make_pair("min-legal-vector-width", "256"));
        } else {
            this->m_funcAttributes.push_back(std::make_pair("prefer-vector-width", "512"));
            this->m_funcAttributes.push_back(std::make_pair("min-legal-vector-width", "512"));
        }
        break;
    case ISPCTarget::avx512skx_i8x64:
#if ISPC_LLVM_VERSION >= ISPC_LLVM_10_0 // LLVM 10.0+
        // This target is enabled only for LLVM 10.0 and later
        // because LLVM requires a number of fixes, which are
        // committed to LLVM 11.0 and can be applied to 10.0, but not
        // earlier versions.
        this->m_isa = Target::SKX_AVX512;
        this->m_nativeVectorWidth = 64;
        this->m_nativeVectorAlignment = 64;
        this->m_dataTypeWidth = 8;
        this->m_vectorWidth = 64;
        this->m_maskingIsFree = true;
        this->m_maskBitCount = 1;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = this->m_hasScatter = true;
        this->m_hasTranscendentals = false;
        this->m_hasTrigonometry = false;
        this->m_hasRsqrtd = this->m_hasRcpd = false;
        this->m_hasVecPrefetch = false;
        CPUfromISA = CPU_SKX;
        break;
#else
        unsupported_target = true;
        break;
#endif
    case ISPCTarget::avx512skx_i16x32:
#if ISPC_LLVM_VERSION >= ISPC_LLVM_10_0 // LLVM 10.0+
        // This target is enabled only for LLVM 10.0 and later
        // because LLVM requires a number of fixes, which are
        // committed to LLVM 11.0 and can be applied to 10.0, but not
        // earlier versions.
        this->m_isa = Target::SKX_AVX512;
        this->m_nativeVectorWidth = 64;
        this->m_nativeVectorAlignment = 64;
        this->m_dataTypeWidth = 16;
        this->m_vectorWidth = 32;
        this->m_maskingIsFree = true;
        this->m_maskBitCount = 1;
        this->m_hasHalf = true;
        this->m_hasRand = true;
        this->m_hasGather = this->m_hasScatter = true;
        this->m_hasTranscendentals = false;
        this->m_hasTrigonometry = false;
        this->m_hasRsqrtd = this->m_hasRcpd = false;
        this->m_hasVecPrefetch = false;
        CPUfromISA = CPU_SKX;
        break;
#else
        unsupported_target = true;
        break;
#endif
#ifdef ISPC_ARM_ENABLED
    case ISPCTarget::neon_i8x16:
        this->m_isa = Target::NEON;
        this->m_nativeVectorWidth = 16;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 8;
        this->m_vectorWidth = 16;
        this->m_hasHalf = true; // ??
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 8;
        break;
    case ISPCTarget::neon_i16x8:
        this->m_isa = Target::NEON;
        this->m_nativeVectorWidth = 8;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 16;
        this->m_vectorWidth = 8;
        this->m_hasHalf = true; // ??
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 16;
        break;
    case ISPCTarget::neon_i32x4:
        this->m_isa = Target::NEON;
        this->m_nativeVectorWidth = 4;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 4;
        this->m_hasHalf = true; // ??
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        break;
    case ISPCTarget::neon_i32x8:
        this->m_isa = Target::NEON;
        this->m_nativeVectorWidth = 4;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 8;
        this->m_hasHalf = true; // ??
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        break;
#else
    case ISPCTarget::neon_i8x16:
    case ISPCTarget::neon_i16x8:
    case ISPCTarget::neon_i32x4:
    case ISPCTarget::neon_i32x8:
        unsupported_target = true;
        break;
#endif
#ifdef ISPC_WASM_ENABLED
    case ISPCTarget::wasm_i32x4:
        this->m_isa = Target::WASM;
        this->m_nativeVectorWidth = 4;
        this->m_nativeVectorAlignment = 16;
        this->m_dataTypeWidth = 32;
        this->m_vectorWidth = 4;
        this->m_hasHalf = false;
        this->m_maskingIsFree = false;
        this->m_maskBitCount = 32;
        this->m_hasTranscendentals = false;
        this->m_hasTrigonometry = false;
        this->m_hasRcpd = false;
        this->m_hasRsqrtd = false;
        this->m_hasScatter = false;
        this->m_hasGather = false;
        this->m_hasVecPrefetch = false;
        break;
#else
    case ISPCTarget::wasm_i32x4:
        unsupported_target = true;
        break;
#endif
    case ISPCTarget::none:
    case ISPCTarget::host:
    case ISPCTarget::error:
        unsupported_target = true;
        break;
    }

    if (unsupported_target) {
        // Hitting one of unsupported targets is internal error.
        // Proper reporting about incorrect targets is done during options parsing.
        std::string target_string = "Problem with target (" + ISPCTargetToString(m_ispc_target) + ")";
        FATAL(target_string.c_str());
    }

#if defined(ISPC_ARM_ENABLED)
    if ((CPUID == CPU_None) && ISPCTargetIsNeon(m_ispc_target)) {
        if (arch == Arch::arm) {
            CPUID = CPU_CortexA9;
        } else if (arch == Arch::aarch64) {
            CPUID = CPU_CortexA35;
        } else {
            UNREACHABLE();
        }
    }
#endif

    if (CPUID == CPU_None) {
        cpu = a.GetDefaultNameFromType(CPUfromISA).c_str();
    } else {
        if ((CPUfromISA != CPU_None) && !a.BackwardCompatible(CPUID, CPUfromISA)) {
            std::string target_string = ISPCTargetToString(m_ispc_target);
            Error(SourcePos(),
                  "The requested CPU (%s) is incompatible"
                  " with the CPU required for %s target (%s)",
                  cpu, target_string.c_str(), a.GetDefaultNameFromType(CPUfromISA).c_str());
            return;
        }
        cpu = a.GetDefaultNameFromType(CPUID).c_str();
    }
    this->m_cpu = cpu;

    if (!error) {
        // Create TargetMachine
        std::string triple = GetTripleString();

        // The last validity check to ensure that supported for this target was enabled in the build.
        if (!g->target_registry->isSupported(m_ispc_target, g->target_os, arch)) {
            std::string target_string = ISPCTargetToString(m_ispc_target);
            std::string arch_str = ArchToString(arch);
            std::string os_str = OSToString(g->target_os);
            Error(SourcePos(), "%s target for %s on %s is not supported in current build.", target_string.c_str(),
                  arch_str.c_str(), os_str.c_str());
            return;
        }

        llvm::Optional<llvm::Reloc::Model> relocModel;
        if (m_generatePIC) {
            relocModel = llvm::Reloc::PIC_;
        }
        llvm::TargetOptions options;
#ifdef ISPC_ARM_ENABLED
        options.FloatABIType = llvm::FloatABI::Hard;
        if (arch == Arch::arm) {
            if (g->target_os == TargetOS::custom_linux) {
                this->m_funcAttributes.push_back(std::make_pair("target-features", "+crypto,+fp-armv8,+neon,+sha2"));
            } else {
                this->m_funcAttributes.push_back(std::make_pair("target-features", "+neon,+fp16"));
            }
            featuresString = "+neon,+fp16";
        } else if (arch == Arch::aarch64) {
            if (g->target_os == TargetOS::custom_linux) {
                this->m_funcAttributes.push_back(
                    std::make_pair("target-features", "+aes,+crc,+crypto,+fp-armv8,+neon,+sha2"));
            } else {
                this->m_funcAttributes.push_back(std::make_pair("target-features", "+neon"));
            }
            featuresString = "+neon";
        }
#endif
        if (g->opt.disableFMA == false)
            options.AllowFPOpFusion = llvm::FPOpFusion::Fast;

        m_targetMachine = m_target->createTargetMachine(triple, m_cpu, featuresString, options, relocModel);
        Assert(m_targetMachine != NULL);

        // Set Optimization level for llvm codegen based on Optimization level
        // requested by user via ISPC Optimization Flag. Mapping is :
        // ISPC O0 -> Codegen O0
        // ISPC O1,O2,O3,default -> Codegen O3
        llvm::CodeGenOpt::Level cOptLevel = llvm::CodeGenOpt::Level::Aggressive;
        switch (g->codegenOptLevel) {
        case Globals::CodegenOptLevel::None:
            cOptLevel = llvm::CodeGenOpt::Level::None;
            break;

        case Globals::CodegenOptLevel::Aggressive:
            cOptLevel = llvm::CodeGenOpt::Level::Aggressive;
            break;
        }
        m_targetMachine->setOptLevel(cOptLevel);

        m_targetMachine->Options.MCOptions.AsmVerbose = true;

        // Change default version of generated DWARF.
        if (g->generateDWARFVersion != 0) {
            m_targetMachine->Options.MCOptions.DwarfVersion = g->generateDWARFVersion;
        }

        // Initialize TargetData/DataLayout in 3 steps.
        // 1. Get default data layout first
        std::string dl_string;
        dl_string = m_targetMachine->createDataLayout().getStringRepresentation();

        // 2. Finally set member data
        m_dataLayout = new llvm::DataLayout(dl_string);

        // Set is32Bit
        // This indicates if we are compiling for 32 bit platform and can assume 32 bit runtime.

        this->m_is32Bit = (getDataLayout()->getPointerSize() == 4);

        // TO-DO : Revisit addition of "target-features" and "target-cpu" for ARM support.
        llvm::AttrBuilder fattrBuilder;
#ifdef ISPC_ARM_ENABLED
        if (m_isa == Target::NEON)
            fattrBuilder.addAttribute("target-cpu", this->m_cpu);
#endif
        for (auto const &f_attr : m_funcAttributes)
            fattrBuilder.addAttribute(f_attr.first, f_attr.second);
        this->m_tf_attributes = new llvm::AttrBuilder(fattrBuilder);

        Assert(this->m_vectorWidth <= ISPC_MAX_NVEC);
    }

    m_valid = !error;

    if (printTarget) {
        printf("Target Triple: %s\n", m_targetMachine->getTargetTriple().str().c_str());
        printf("Target CPU: %s\n", m_targetMachine->getTargetCPU().str().c_str());
        printf("Target Feature String: %s\n", m_targetMachine->getTargetFeatureString().str().c_str());
    }

    return;
}

std::string Target::SupportedCPUs() {
    AllCPUs a;
    return a.HumanReadableListOfNames();
}

std::string Target::GetTripleString() const {
    llvm::Triple triple;
    switch (g->target_os) {
    case TargetOS::windows:
        if (m_arch == Arch::x86) {
            triple.setArchName("i686");
        } else if (m_arch == Arch::x86_64) {
            triple.setArchName("x86_64");
        } else if (m_arch == Arch::arm) {
            Error(SourcePos(), "Arm is not supported on Windows.");
            exit(1);
        } else if (m_arch == Arch::aarch64) {
            Error(SourcePos(), "Aarch64 is not supported on Windows.");
            exit(1);
        } else {
            Error(SourcePos(), "Unknown arch.");
            exit(1);
        }
        //"x86_64-pc-windows-msvc"
        triple.setVendor(llvm::Triple::VendorType::PC);
        triple.setOS(llvm::Triple::OSType::Win32);
        triple.setEnvironment(llvm::Triple::EnvironmentType::MSVC);
        break;
    case TargetOS::custom_linux:
    case TargetOS::linux:
        if (m_arch == Arch::x86) {
            triple.setArchName("i686");
        } else if (m_arch == Arch::x86_64) {
            triple.setArchName("x86_64");
        } else if (m_arch == Arch::arm) {
            triple.setArchName("armv7");
        } else if (m_arch == Arch::aarch64) {
            triple.setArchName("aarch64");
        } else {
            Error(SourcePos(), "Unknown arch.");
            exit(1);
        }
        triple.setVendor(llvm::Triple::VendorType::UnknownVendor);
        triple.setOS(llvm::Triple::OSType::Linux);
        if (m_arch == Arch::x86 || m_arch == Arch::x86_64 || m_arch == Arch::aarch64) {
            triple.setEnvironment(llvm::Triple::EnvironmentType::GNU);
        } else if (m_arch == Arch::arm) {
            triple.setEnvironment(llvm::Triple::EnvironmentType::GNUEABIHF);
        } else {
            Error(SourcePos(), "Unknown arch.");
            exit(1);
        }
        break;
    case TargetOS::freebsd:
        if (m_arch == Arch::x86) {
            triple.setArchName("i686");
        } else if (m_arch == Arch::x86_64) {
            triple.setArchName("amd64");
        } else if (m_arch == Arch::arm) {
            triple.setArchName("armv7");
        } else if (m_arch == Arch::aarch64) {
            triple.setArchName("aarch64");
        } else {
            Error(SourcePos(), "Unknown arch.");
            exit(1);
        }
        triple.setVendor(llvm::Triple::VendorType::UnknownVendor);
        triple.setOS(llvm::Triple::OSType::FreeBSD);
        break;
    case TargetOS::macos:
        // asserts
        if (m_arch != Arch::x86_64) {
            Error(SourcePos(), "macOS target supports only x86_64.");
            exit(1);
        }
        triple.setArch(llvm::Triple::ArchType::x86_64);
        triple.setVendor(llvm::Triple::VendorType::Apple);
        triple.setOS(llvm::Triple::OSType::MacOSX);
        break;
    case TargetOS::android:
        if (m_arch == Arch::x86) {
            triple.setArchName("i686");
        } else if (m_arch == Arch::x86_64) {
            triple.setArchName("x86_64");
        } else if (m_arch == Arch::arm) {
            triple.setArchName("armv7");
        } else if (m_arch == Arch::aarch64) {
            triple.setArchName("aarch64");
        } else {
            Error(SourcePos(), "Unknown arch.");
            exit(1);
        }
        triple.setVendor(llvm::Triple::VendorType::UnknownVendor);
        triple.setOS(llvm::Triple::OSType::Linux);
        triple.setEnvironment(llvm::Triple::EnvironmentType::Android);
        break;
    case TargetOS::ios:
        if (m_arch != Arch::aarch64) {
            Error(SourcePos(), "iOS target supports only aarch64.");
            exit(1);
        }
        // Note, for iOS arch need to be set to "arm64", instead of "aarch64".
        // Internet say this is for historical reasons.
        // "arm64-apple-ios"
        triple.setArchName("arm64");
        triple.setVendor(llvm::Triple::VendorType::Apple);
        triple.setOS(llvm::Triple::OSType::IOS);
        break;
    case TargetOS::ps4:
        if (m_arch != Arch::x86_64) {
            Error(SourcePos(), "PS4 target supports only x86_64.");
            exit(1);
        }
        // "x86_64-scei-ps4"
        triple.setArch(llvm::Triple::ArchType::x86_64);
        triple.setVendor(llvm::Triple::VendorType::SCEI);
        triple.setOS(llvm::Triple::OSType::PS4);
        break;
    case TargetOS::web:
        if (m_arch != Arch::wasm32) {
            Error(SourcePos(), "Web target supports only wasm32.");
            exit(1);
        }
        triple.setArch(llvm::Triple::ArchType::wasm32);
        triple.setVendor(llvm::Triple::VendorType::UnknownVendor);
        triple.setOS(llvm::Triple::OSType::UnknownOS);
        break;
    case TargetOS::error:
        Error(SourcePos(), "Invalid target OS.");
        exit(1);
    }

    return triple.str();
}

// This function returns string representation of ISA for the purpose of
// mangling. And may return any unique string, preferably short, like
// sse4, avx and etc.
const char *Target::ISAToString(ISA isa) {
    switch (isa) {
#ifdef ISPC_ARM_ENABLED
    case Target::NEON:
        return "neon";
#endif
#ifdef ISPC_WASM_ENABLED
    case Target::WASM:
        return "wasm";
#endif
    case Target::SSE2:
        return "sse2";
    case Target::SSE4:
        return "sse4";
    case Target::AVX:
        return "avx";
    case Target::AVX2:
        return "avx2";
    case Target::KNL_AVX512:
        return "avx512knl";
    case Target::SKX_AVX512:
        return "avx512skx";
    default:
        FATAL("Unhandled target in ISAToString()");
    }
    return "";
}

const char *Target::GetISAString() const { return ISAToString(m_isa); }

// This function returns string representation of default target corresponding
// to ISA. I.e. for SSE4 it's sse4-i32x4, for AVX2 it's avx2-i32x8. This
// string may be used to initialize Target.
const char *Target::ISAToTargetString(ISA isa) {
    switch (isa) {
#ifdef ISPC_ARM_ENABLED
    case Target::NEON:
        return "neon-i32x4";
#endif
#ifdef ISPC_WASM_ENABLED
    case Target::WASM:
        return "wasm-i32x4";
#endif
    case Target::SSE2:
        return "sse2-i32x4";
    case Target::SSE4:
        return "sse4-i32x4";
    case Target::AVX:
        return "avx1-i32x8";
    case Target::AVX2:
        return "avx2-i32x8";
    case Target::KNL_AVX512:
        return "avx512knl-i32x16";
    case Target::SKX_AVX512:
        return "avx512skx-i32x16";
    default:
        FATAL("Unhandled target in ISAToTargetString()");
    }
    return "";
}

const char *Target::GetISATargetString() const { return ISAToTargetString(m_isa); }

llvm::Value *Target::SizeOf(llvm::Type *type, llvm::BasicBlock *insertAtEnd) {
    uint64_t byteSize = getDataLayout()->getTypeStoreSize(type);
    if (m_is32Bit || g->opt.force32BitAddressing)
        return LLVMInt32((int32_t)byteSize);
    else
        return LLVMInt64(byteSize);
}

llvm::Value *Target::StructOffset(llvm::Type *type, int element, llvm::BasicBlock *insertAtEnd) {
    llvm::StructType *structType = llvm::dyn_cast<llvm::StructType>(type);
    if (structType == NULL || structType->isSized() == false) {
        Assert(m->errorCount > 0);
        return NULL;
    }

    const llvm::StructLayout *sl = getDataLayout()->getStructLayout(structType);
    Assert(sl != NULL);

    uint64_t offset = sl->getElementOffset(element);
    if (m_is32Bit || g->opt.force32BitAddressing)
        return LLVMInt32((int32_t)offset);
    else
        return LLVMInt64(offset);
}

void Target::markFuncWithTargetAttr(llvm::Function *func) {
    if (m_tf_attributes) {
        func->addAttributes(llvm::AttributeList::FunctionIndex, *m_tf_attributes);
    }
}


/// <summary>
/// ////////////////////////////////
struct CCState {
    CCState(int argSize) : IsPreassigned(argSize) {}

    llvm::SmallBitVector IsPreassigned;
    unsigned FreeRegs = 0;
    unsigned FreeSSERegs = 0;
};


/// Returns true if this aggregate is small enough to be passed in SSE registers
/// in the X86_VectorCall calling convention. Shared between x86_32 and x86_64.
static bool isX86VectorCallAggregateSmallEnough(uint64_t NumMembers) { return NumMembers <= 4; }

/// Returns true if this type can be passed in SSE registers with the
/// X86_VectorCall calling convention. Shared between x86_32 and x86_64.
static bool isX86VectorTypeForVectorCall(llvm::Type *argType) {

    if (argType->isFloatTy() || argType->isDoubleTy()) {
        return true;
    } else if (argType->isVectorTy()) {
        unsigned VecSize = g->target->getDataLayout()->getTypeSizeInBits(argType);
        if (VecSize == 128 || VecSize == 256 || VecSize == 512)
            return true;
    }
    return false;
}

static bool isEmptyRecord(llvm::Type *type, bool AllowArrays);
    /// isEmptyField - Return true iff a the field is "empty", that is it
/// is an unnamed bit-field or an (array of) empty record(s).
static bool isEmptyField(llvm::Type *type, bool AllowArrays) {

    /* Do not support anonymous*/
    /*if (FD->isUnnamedBitfield())
        return true;
    */

    if (AllowArrays)
        while (llvm::ArrayType *arrType = llvm::dyn_cast<llvm::ArrayType>(type)) {
            if (g->target->getDataLayout()->getTypeSizeInBits(arrType->getArrayElementType()))
                return false;
            type = arrType->getArrayElementType();
        }

    if (!type->isStructTy())
        return false;

    return isEmptyRecord(type, AllowArrays);

}

/// isEmptyRecord - Return true iff a structure contains only empty
/// fields. Note that a structure with a flexible array member is not
/// considered empty.
static bool isEmptyRecord(llvm::Type *type, bool AllowArrays) {
    llvm::StructType *strctType = llvm::dyn_cast<llvm::StructType>(type);
    if (!strctType)
        return false;

    int numElems = strctType->getNumElements();
    for (int id = 0; id < numElems; id++) {
        llvm::Type *elemType = strctType->getElementType(id);
        if (!isEmptyField(elemType, AllowArrays))
            return false;
    }

    return true;
}

/// isHomogeneousAggregate - Return true if a type is an ELFv2 homogeneous
/// aggregate.  Base is set to the base element type, and Members is set
/// to the number of base elements.
bool isHomogeneousAggregate(llvm::Type *type, llvm::Type *&Base, uint64_t &Members) {
    printf("\n isHomogeneousAggregate ENTER\n");
    type->dump();
    if (type->isArrayTy()) {
        // if (llvm::ConstantArray *cArr = llvm::dyn_cast<llvm::ConstantArray>(type)) {??
        uint64_t NElements = type->getArrayNumElements();

        if (NElements == 0)
            return false;
        if (!isHomogeneousAggregate(type->getArrayElementType(), Base, Members))
            return false;
        Members *= NElements;
    } else if (llvm::StructType *strctType = llvm::dyn_cast<llvm::StructType>(type)) {
        int numElems = strctType->getNumElements();
        for (int id = 0; id < numElems; id++) {
            llvm::Type *elemType = strctType->getElementType(id);
            while (llvm::ArrayType *arrType = llvm::dyn_cast<llvm::ArrayType>(elemType)) {
                if (g->target->getDataLayout()->getTypeSizeInBits(arrType->getArrayElementType()) == 0)
                    return false;
                elemType = arrType->getArrayElementType();
            }
            if (isEmptyRecord(elemType, true))
                continue;

            uint64_t FldMembers;
            if (!isHomogeneousAggregate(strctType->getElementType(id), Base, FldMembers))
                return false;

            Members = Members + FldMembers;
        }

        if (!Base)
            return false;
		Base->dump();
		printf("\n g->target->getDataLayout()->getTypeSizeInBits(Base) = %d n",
			(int)g->target->getDataLayout()->getTypeSizeInBits(Base));
		type->dump();
		printf("\n g->target->getDataLayout()->getTypeSizeInBits(type) = %d n",
			(int)g->target->getDataLayout()->getTypeSizeInBits(type));
        // Ensure there is no padding.
        if (g->target->getDataLayout()->getTypeSizeInBits(Base) * Members !=
            g->target->getDataLayout()->getTypeSizeInBits(type))
            return false;

    } else {
        Members = 1;

        // Most ABIs only support float, double, and some vector type widths.
        if (!isX86VectorTypeForVectorCall(type)) {
            printf("\n !isX86VectorTypeForVectorCall(type) \n");
            return false;
        }
            

        // The base type must be the same for all members.  Types that
        // agree in both total size and mode (float vs. vector) are
        // treated as being equivalent here.
        // Revisit
        /*const Type *TyPtr = Ty.getTypePtr();*/
        if (!Base) {
            printf("\n !!Base(type) \n");
            Base = type;
            // If it's a non-power-of-2 vector, its size is already a power-of-2,
            // so make sure to widen it explicitly.
            /*if (const VectorType *VT = Base->getAs<VectorType>()) {
                QualType EltTy = VT->getElementType();
                unsigned NumElements = getContext().getTypeSize(VT) / getContext().getTypeSize(EltTy);
                Base = getContext().getVectorType(EltTy, NumElements, VT->getVectorKind()).getTypePtr();
            }*/
        }

        if (Base->isVectorTy() != type->isVectorTy() ||
            g->target->getDataLayout()->getTypeSizeInBits(Base) != g->target->getDataLayout()->getTypeSizeInBits(type))
            return false;
    }
    return Members > 0 && isX86VectorCallAggregateSmallEnough(Members);
}




static unsigned getTypeStackAlignInBytes(unsigned Align) {
	// Otherwise, if the alignment is less than or equal to the minimum ABI
	// alignment, just use the default; the backend will handle this.
	unsigned MinABIStackAlignInBytes = 4;
	if (Align <= MinABIStackAlignInBytes)
		return 0; // Use default alignment.

	// On non-Darwin, the stack type alignment is always 4.
	return MinABIStackAlignInBytes;
}

ABIArgInfo getIndirectResult(llvm::Type *type, bool ByVal, CCState &State) {
    if (!ByVal) {
        if (State.FreeRegs) {
            --State.FreeRegs; // Non-byval indirects just use one pointer.
            //return getNaturalAlignIndirectInReg(Ty);
			/*return ABIArgInfo::getIndirectInReg(getContext().getTypeAlignInChars(Ty),
				false, false);*/
			return ABIArgInfo::getIndirectInReg(4,
				false, false);
        }
        //return getNaturalAlignIndirect(Ty, false);
		/*return ABIArgInfo::getIndirect(getContext().getTypeAlignInChars(Ty),
			false, false, nullptr);*/
        return ABIArgInfo::getIndirect(4, false, false, nullptr);
    }

    // Compute the byval alignment.
    unsigned TypeAlign = g->target->getDataLayout()->getABITypeAlignment(type); 
    // g->target->getDataLayout()->getABITypeAlignment(type).value / 8;
    unsigned StackAlign = getTypeStackAlignInBytes(TypeAlign);
    if (StackAlign == 0)
        return ABIArgInfo::getIndirect(4, /*ByVal=*/true);

    // If the stack alignment is less than the type alignment, realign the
    // argument.
    bool Realign = TypeAlign > StackAlign;
    return ABIArgInfo::getIndirect(StackAlign,
                                   /*ByVal=*/true, Realign);
}

// Returns a Homogeneous Vector Aggregate ABIArgInfo, used in X86.
static ABIArgInfo getDirectX86Hva(llvm::Type *T = nullptr) {
    auto AI = ABIArgInfo::getDirectInReg(T);
    /*AI.setInReg(true);
    AI.setCanBeFlattened(false);*/
    return AI;
}

bool isFunctonPtrTy(llvm::Type * type) {
	llvm::Type *ptr = type;
	while (ptr->isPointerTy()) {
		ptr = ptr->getPointerElementType();
		if (ptr->isFunctionTy())
			return true;
	}
	return false;
}


llvm::Type* isSingleElementStruct(llvm::Type *type) {

	llvm::StructType *strctType = llvm::dyn_cast<llvm::StructType>(type);
	if (!strctType)
		return nullptr;

	llvm::Type *found = nullptr;

	int numElems = strctType->getNumElements();
	for (int id = 0; id < numElems; id++) {

		llvm::Type *elemType = strctType->getElementType(id);

		// Ignore empty fields.
		if (isEmptyRecord(elemType, true))
			continue;


		// If we already found an element then this isn't a single-element
		// struct.
		if (found)
			return nullptr;

		// Treat single element arrays as the element.
		while (llvm::ArrayType *AT = llvm::dyn_cast<llvm::ArrayType>(elemType)) {
			if (AT->getArrayNumElements() != 1)
				break;
			elemType = AT->getElementType();
		}

		// isAggregateTypeForABI
		if (!(elemType->isVectorTy() || elemType->isStructTy() || elemType->isPointerTy() && isFunctonPtrTy(elemType))) {
			//Found = FT.getTypePtr(); REVISIT
			found = elemType->getPointerElementType();
		}
		else {
			found = isSingleElementStruct(elemType);
			if (!found)
				return nullptr;
		}
		
	}

	// We don't consider a struct a single-element struct if it has
  // padding beyond the element type.
	if (found && (g->target->getDataLayout()->getTypeSizeInBits(found) != g->target->getDataLayout()->getTypeSizeInBits(type)))
		return nullptr;

	return found;

}
bool classify(llvm::Type *type) {

	bool isFloat = false;

	llvm::Type *relType = isSingleElementStruct(type);
	if (!relType) {
		relType = type;
		// T = Ty.getTypePtr(); REVISIT
	}

	if (relType->isDoubleTy() || relType->isFloatTy())
		isFloat = true;

	return isFloat;
}
bool updateFreeRegs(llvm::Type *type, CCState &State) {

	if (classify(type))
		return false;

	unsigned Size = g->target->getDataLayout()->getTypeSizeInBits(type);
	unsigned SizeInRegs = (Size + 31) / 32;

	if (SizeInRegs == 0)
		return false;

	if (SizeInRegs > State.FreeRegs) {
		State.FreeRegs = 0;
		return false;
	}

	State.FreeRegs -= SizeInRegs;
	return true;
}

bool shouldAggregateUseDirect(llvm::Type *type, CCState &State,
	bool &InReg,
	bool &NeedsPadding) {
	// On Windows, aggregates other than HFAs are never passed in registers, and
	// they do not consume register slots. Homogenous floating-point aggregates
	// (HFAs) have already been dealt with at this point.
	// isAggregateTypeForABI
	if (type->isVectorTy() || type->isStructTy() || (type->isPointerTy() && isFunctonPtrTy(type))) {
		return false;
	}

	NeedsPadding = false;
	InReg = true;

	if (!updateFreeRegs(type, State))
		return false;

	if (g->target->getDataLayout()->getTypeSizeInBits(type) <= 32 && State.FreeRegs)
		NeedsPadding = true;
	
	return false;

}


bool addFieldSizes(llvm::StructType *strctType, uint64_t &size) {

	int numElems = strctType->getNumElements();
	for (int id = 0; id < numElems; id++) {

		llvm::Type *elemType = strctType->getElementType(id);
		// Scalar arguments on the stack get 4 byte alignment on x86. If the
		// argument is smaller than 32-bits, expanding the struct will create
		// alignment padding.
		uint64_t eSize = g->target->getDataLayout()->getTypeSizeInBits(elemType);
		if (elemType->isArrayTy() || elemType->isVectorTy() || elemType->isStructTy()) {			
			if (!(eSize == 32 || eSize == 64))
				return false;

		}

		size += eSize;			
	}

	return true;

}

/// Test whether an argument type which is to be passed indirectly (on the
/// stack) would have the equivalent layout if it was expanded into separate
/// arguments. If so, we prefer to do the latter to avoid inhibiting
/// optimizations.
bool canExpandIndirectArgument(llvm::Type *type)  {
	// We can only expand structure types.
	llvm::StructType *strType = llvm::dyn_cast<llvm::StructType>(type);
	if (!strType)
		return false;

	uint64_t size = 0;
	if (!addFieldSizes(strType, size))
		return false;

	return size == g->target->getDataLayout()->getTypeSizeInBits(type);

}

bool shouldPrimitiveUseInReg(llvm::Type *type, CCState &State) {
	if (!updateFreeRegs(type, State))
		return false;

	if (g->target->getDataLayout()->getTypeSizeInBits(type) > 32) {
		return false;
	}

	return (type->isIntegerTy() || type->isPointerTy());

}

/*ABIArgInfo*/ ABIArgInfo classifyArgumentType(llvm::Type *type, CCState &State) { 
    
    // Regcall uses the concept of a homogenous vector aggregate, similar
    // to other targets.
    printf("\n classifyArgumentType \n");
    type->dump();
    llvm::Type *Base = nullptr;
    uint64_t NumElts = 0;
    if ( isHomogeneousAggregate(type, Base, NumElts)) {
        if (State.FreeSSERegs >= NumElts) {
            State.FreeSSERegs -= NumElts;

            // Vectorcall passes HVAs directly and does not flatten them, but regcall
            // does.
            printf("\n getDirectX86Hva 1708 \n");
            return getDirectX86Hva();

        }
        printf("\n getIndirectResult 1712 \n");
        return getIndirectResult(type, /*ByVal=*/false, State);
    }

	// isAggregateTypeForABI
	if (type->isArrayTy() || type->isStructTy() || type->isPointerTy() && isFunctonPtrTy(type)) {

		// Ignore empty structs/unions on non-Windows.
        if (isEmptyRecord(type, true)) {
            printf("\n getIgnore 1721 \n");
            return ABIArgInfo::getIgnore();
        }
			

		bool NeedsPadding = false;
		bool InReg;
		llvm::IntegerType *Int32 = llvm::Type::getInt32Ty(*g->ctx);
		if (shouldAggregateUseDirect(type, State, InReg, NeedsPadding)) {
			unsigned SizeInRegs = (g->target->getDataLayout()->getTypeSizeInBits(type) + 31) / 32;
			llvm::SmallVector<llvm::Type*, 3> Elements(SizeInRegs, Int32);
            llvm::Type *Result = llvm::StructType::get(*g->ctx, Elements);
            if (InReg) {
                printf("\n getDirectInReg 1734 \n");
                return ABIArgInfo::getDirectInReg(Result);
            }
				
			else {
                printf("\n getDirect 1739 \n");
                return ABIArgInfo::getDirect(Result);
            }
				
		}

		llvm::IntegerType *PaddingType = NeedsPadding ? Int32 : nullptr;

		// Pass over-aligned aggregates on Windows indirectly. This behavior was
		// added in MSVC 2015.
		/*if (IsWin32StructABI && TI.AlignIsRequired && TI.Align > 32)
			return getIndirectResult(Ty, false, State);*/ //REVISIT

    // Expand small (<= 128-bit) record types when we know that the stack layout
	// of those arguments will match the struct. This is important because the
	// LLVM backend isn't smart enough to remove byval, which inhibits many
	// optimizations.
	// Don't do this for the MCU if there are still free integer registers
	// (see X86_64 ABI for full explanation).
		printf("\n g->target->getDataLayout()->getTypeSizeInBits(type) = %d \n", (int)g->target->getDataLayout()->getTypeSizeInBits(type));
        if (g->target->getDataLayout()->getTypeSizeInBits(type) <= 4 * 32 &&
            canExpandIndirectArgument(type)) {
            printf("\n getExpandWithPadding 1760 \n");
            return ABIArgInfo::getExpandWithPadding(true, PaddingType);
        }
        printf("\n getIndirectResult 1763 \n");
		return getIndirectResult(type, true, State);

	}

	if (llvm::VectorType *vt = llvm::dyn_cast<llvm::VectorType>(type)) {
		if (g->target->getDataLayout()->getTypeSizeInBits(vt) <= 512 && State.FreeSSERegs > 0) {
			--State.FreeSSERegs;
            printf("\n getDirectInReg 1771 \n");
			return ABIArgInfo::getDirectInReg();
		}
        printf("\n getIndirectResult 1774 \n");
		return getIndirectResult(vt, /*ByVal=*/false, State);

	}

	bool InReg = shouldPrimitiveUseInReg(type, State);

    // Already promoted
    /*if (type->isIntegerTy() && (g->target->getDataLayout()->getTypeSizeInBits(type) < 32)) {
        if (InReg) {
            printf("\n getExtendInReg 1782 \n");
            return ABIArgInfo::getExtendInReg(*type);
        }
        return ABIArgInfo::getExtend(*type);
    }*/
	

	if (InReg) {
        printf("\n getDirectInReg 1788 \n");
		return ABIArgInfo::getDirectInReg();
	}
    printf("\n getDirect 1791 \n");
	return ABIArgInfo::getDirect();    

}



static void runVectorCallFirstPass(llvm::FunctionType *fType, CCState &state, std::vector<ABIArgInfo> &argInfo) {
    // Vectorcall x86 works subtly different than in x64, so the format is
    // a bit different than the x64 version.  First, all vector types (not HVAs)
    // are assigned, with the first 6 ending up in the [XYZ]MM0-5 registers.
    // This differs from the x64 implementation, where the first 6 by INDEX get
    // registers.
    // In the second pass over the arguments, HVAs are passed in the remaining
    // vector registers if possible, or indirectly by address. The address will be
    // passed in ECX/EDX if available. Any other arguments are passed according to
    // the usual fastcall rules.
    //llvm::Function::arg_iterator argIter = func->arg_begin();
    //llvm::FunctionType *fType = func->getFunctionType();
    printf("\n ENTER runVectorCallFirstPass \n");
    for (int paramNum = 0; paramNum < fType->getNumParams(); paramNum++) {
        llvm::Type *Base = nullptr;
        uint64_t NumElts = 0;
        
        llvm::Type *argType = fType->getParamType(paramNum);
        printf("\n ENTER arg ParamNO = %d", (int)paramNum);
        argType->dump();
        if ((argType->isVectorTy() || argType->isIntegerTy() || argType->isFloatTy() || argType->isDoubleTy()) &&
            isHomogeneousAggregate(argType, Base, NumElts)) {
            if (state.FreeSSERegs >= NumElts) {
                state.FreeSSERegs -= NumElts;
                state.IsPreassigned.set(paramNum);
                argInfo[paramNum] = ABIArgInfo::getDirectInReg();
            }
        }
    }
}

void Target::computeInfo(llvm::FunctionType *fType, std::vector<ABIArgInfo> &argInfo) {
    printf("\n ENTER Target::computeInfo \n");
    unsigned int argNum = fType->getNumParams();
    CCState State(argNum);
    State.FreeRegs = 2;
    State.FreeSSERegs = 6;

    runVectorCallFirstPass(fType, State, argInfo);

    bool UsedInAlloca = false;

    for (int paramNum = 0; paramNum < fType->getNumParams(); paramNum++) {
    //llvm::Function::arg_iterator argIter = func->arg_begin();
    //for (; argIter != func->arg_end(); ++argIter) {
        // Skip arguments that have already been assigned.
        if (State.IsPreassigned.test(paramNum))
            continue;

        argInfo[paramNum] = classifyArgumentType(fType->getParamType(paramNum), State);
        //UsedInAlloca |= (Args[I].info.getKind() == ABIArgInfo::InAlloca);
    }
}

void Target::setInRegForFunction(llvm::Function *function, std::vector<ABIArgInfo> &argInfo) {

    llvm::FunctionType *fType = function->getFunctionType();
    printf("\n setInRegForFunction \n");
    fType->dump();

    for (int paramNum = 0; paramNum < fType->getNumParams(); paramNum++) {
        printf("\n Adding Attr paramNum = %d, nArgs = %d \n", paramNum, fType->getNumParams());
        argInfo[paramNum].print();
        if (argInfo[paramNum].getInReg()) {

            function->addParamAttr(paramNum, llvm::Attribute::InReg);
        }
    }

}

void Target::markFuncWithCallingConv(llvm::Function *func) {
    assert(g->calling_conv != CallingConv::uninitialized);
    if (g->calling_conv == CallingConv::x86_vectorcall) {
        func->setCallingConv(llvm::CallingConv::X86_VectorCall);
        

        // Add x86 vectorcall changes as a separate commit.
        /*
        // We have to jump through some hoops for x86.
        // In LLVM IR for x86, arguments which are to be passed in registers
        // have to marked with 'InReg' attribue.
        // Rules(Ref : https://docs.microsoft.com/en-us/cpp/cpp/vectorcall?view=vs-2019 )
        // Definitions:
        // Integer Type : it fits in the native register size of the processor�for example,
        // 4 bytes on an x86 machine.Integer types include pointer, reference, and struct or union types of 4 bytes or
        less.
        // Vector Type : either a floating - point type�for example, a float or double�or an SIMD vector type�for
        // example, __m128 or __m256.
        // Rules for x86: Integer Type : The first two integer type arguments found in the
        // parameter list from left to right are placed in ECX and EDX, respectively.
        // Vector Type : The first six vector type arguments in order from left to right are passed by value in SSE
        vector registers 0 to 5.
        //The seventh and subsequent vector type arguments are passed on the stack by reference to memory allocated by
        the caller.
        // Observations from Clang(Is there somewhere these rules are mentioned??)
        // Integer Type : After first Integer Type greater than 32 bit, other integer types NOT passed in reg.
        // Vector Type : After 6 Vector Type args, if 2 Integer Type registers are not yet used, VectorType args
        // passed by reference via register - TO DO

        if (m_arch == Arch::x86) {
            llvm::Function::arg_iterator argIter = func->arg_begin();
            llvm::FunctionType *fType = func->getFunctionType();
            int numArgsVecInReg = 0;
            int numArgsIntInReg = 0;
            for (; argIter != func->arg_end(); ++argIter) {
                llvm::Type *argType = fType->getParamType(argIter->getArgNo());
                if (argType->isIntegerTy() || argType->isStructTy() || argType->isPointerTy()) {
                    if (((argType->isIntegerTy()) || (argType->isStructTy())) &&
                        (g->target->getDataLayout()->getTypeSizeInBits(argType) > 32)) {
                        numArgsIntInReg = 2;
                        continue;
                    }

                    numArgsIntInReg++;
                    argIter->addAttr(llvm::Attribute::InReg);
                    continue;
                }
                if (((llvm::dyn_cast<llvm::VectorType>(argType) != NULL) || argType->isFloatTy() ||
                     argType->isDoubleTy())) {
                    numArgsVecInReg++;
                    argIter->addAttr(llvm::Attribute::InReg);
                }

                if ((numArgsIntInReg == 2) && (numArgsVecInReg == 6))
                    break;
            }
        }*/
    }
}

///////////////////////////////////////////////////////////////////////////
// Opt

Opt::Opt() {
    level = 1;
    fastMath = false;
    fastMaskedVload = false;
    force32BitAddressing = true;
    unrollLoops = true;
    disableAsserts = false;
    disableFMA = false;
    forceAlignedMemory = false;
    disableMaskAllOnOptimizations = false;
    disableHandlePseudoMemoryOps = false;
    disableBlendedMaskedStores = false;
    disableCoherentControlFlow = false;
    disableUniformControlFlow = false;
    disableGatherScatterOptimizations = false;
    disableMaskedStoreToStore = false;
    disableGatherScatterFlattening = false;
    disableUniformMemoryOptimizations = false;
    disableCoalescing = false;
    disableZMM = false;
}

///////////////////////////////////////////////////////////////////////////
// Globals

Globals::Globals() {
    target_registry = TargetLibRegistry::getTargetLibRegistry();

    mathLib = Globals::Math_ISPC;
    codegenOptLevel = Globals::Aggressive;

    includeStdlib = true;
    runCPP = true;
    debugPrint = false;
    dumpFile = false;
    printTarget = false;
    NoOmitFramePointer = false;
    debugIR = -1;
    disableWarnings = false;
    warningsAsErrors = false;
    quiet = false;
    forceColoredOutput = false;
    disableLineWrap = false;
    emitPerfWarnings = true;
    emitInstrumentation = false;
    noPragmaOnce = false;
    generateDebuggingSymbols = false;
    generateDWARFVersion = 3;
    enableFuzzTest = false;
    fuzzTestSeed = -1;
    mangleFunctionsWithTarget = false;
    isMultiTargetCompilation = false;
    errorLimit = -1;
    target = NULL;
    ctx = new llvm::LLVMContext;

#ifdef ISPC_HOST_IS_WINDOWS
    _getcwd(currentDirectory, sizeof(currentDirectory));
#else
    if (getcwd(currentDirectory, sizeof(currentDirectory)) == NULL)
        FATAL("Current directory path is too long!");
#endif
    forceAlignment = -1;
    dllExport = false;

    // Target OS defaults to host OS.
    target_os = GetHostOS();

    // Set calling convention to 'uninitialized'.
    // This needs to be set once target OS is decided.
    calling_conv = CallingConv::uninitialized;

    abiInfo = ABIInfo::uninitialized;
}

///////////////////////////////////////////////////////////////////////////
// SourcePos

SourcePos::SourcePos(const char *n, int fl, int fc, int ll, int lc) {
    name = n;
    if (name == NULL) {
        if (m != NULL)
            name = m->module->getModuleIdentifier().c_str();
        else
            name = "(unknown)";
    }
    first_line = fl;
    first_column = fc;
    last_line = ll != 0 ? ll : fl;
    last_column = lc != 0 ? lc : fc;
}

llvm::DIFile *
// llvm::MDFile*
SourcePos::GetDIFile() const {
    std::string directory, filename;
    GetDirectoryAndFileName(g->currentDirectory, name, &directory, &filename);
    llvm::DIFile *ret = m->diBuilder->createFile(filename, directory);
    return ret;
}

llvm::DINamespace *SourcePos::GetDINamespace() const {
    llvm::DIScope *discope = GetDIFile();
    llvm::DINamespace *ret = m->diBuilder->createNameSpace(discope, "ispc", true);
    return ret;
}

void SourcePos::Print() const {
    printf(" @ [%s:%d.%d - %d.%d] ", name, first_line, first_column, last_line, last_column);
}

bool SourcePos::operator==(const SourcePos &p2) const {
    return (!strcmp(name, p2.name) && first_line == p2.first_line && first_column == p2.first_column &&
            last_line == p2.last_line && last_column == p2.last_column);
}

SourcePos Union(const SourcePos &p1, const SourcePos &p2) {
    if (strcmp(p1.name, p2.name) != 0)
        return p1;

    SourcePos ret;
    ret.name = p1.name;
    ret.first_line = std::min(p1.first_line, p2.first_line);
    ret.first_column = std::min(p1.first_column, p2.first_column);
    ret.last_line = std::max(p1.last_line, p2.last_line);
    ret.last_column = std::max(p1.last_column, p2.last_column);
    return ret;
}
