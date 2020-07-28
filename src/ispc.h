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

/** @file ispc.h
    @brief Main ispc.header file. Defines Target, Globals and Opt classes.
*/

#pragma once

#include "ispc_version.h"
#include "target_enums.h"
#include "target_registry.h"

#if ISPC_LLVM_VERSION < OLDEST_SUPPORTED_LLVM || ISPC_LLVM_VERSION > LATEST_SUPPORTED_LLVM
#error "Only LLVM 8.0 - 11.0 and 12.0 development branch are supported"
#endif

#if defined(_WIN32) || defined(_WIN64)
#define ISPC_HOST_IS_WINDOWS
#elif defined(__linux__)
#define ISPC_HOST_IS_LINUX
#elif defined(__FreeBSD__)
#define ISPC_HOST_IS_FREEBSD
#elif defined(__APPLE__)
#define ISPC_HOST_IS_APPLE
#endif

#include <map>
#include <set>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>

#include "llvm/IR/DerivedTypes.h"

/** @def ISPC_MAX_NVEC maximum vector size of any of the compliation
    targets.
 */
#define ISPC_MAX_NVEC 64

// Number of final optimization phase
#define LAST_OPT_NUMBER 1000

// Forward declarations of a number of widely-used LLVM types
namespace llvm {

class AttrBuilder;
class BasicBlock;
class Constant;
class ConstantValue;
class DataLayout;
class DIBuilder;
class Function;
class FunctionType;
class LLVMContext;
class Module;
class Target;
class TargetMachine;
class Type;
class Value;
class DIFile;
class DINamespace;
class DIType;

class DIScope;
} // namespace llvm

class ArrayType;
class AST;
class ASTNode;
class AtomicType;
class FunctionEmitContext;
class Expr;
class ExprList;
class Function;
class FunctionType;
class Module;
class PointerType;
class Stmt;
class Symbol;
class SymbolTable;
class Type;
struct VariableDeclaration;

enum StorageClass { SC_NONE, SC_EXTERN, SC_STATIC, SC_TYPEDEF, SC_EXTERN_C };

/** @brief Representation of a range of positions in a source file.

    This class represents a range of characters in a source file
    (e.g. those that span a token's definition), from starting line and
    column to ending line and column.  (These values are tracked by the
    lexing code).  Both lines and columns are counted starting from one.
 */
struct SourcePos {
    SourcePos(const char *n = NULL, int fl = 0, int fc = 0, int ll = 0, int lc = 0);

    const char *name;
    int first_line;
    int first_column;
    int last_line;
    int last_column;

    /** Prints the filename and line/column range to standard output. */
    void Print() const;

    /** Returns a LLVM DIFile object that represents the SourcePos's file */
    llvm::DIFile *GetDIFile() const;

    /** Returns a LLVM DINamespace object that represents 'ispc' namespace. */
    llvm::DINamespace *GetDINamespace() const;

    bool operator==(const SourcePos &p2) const;
};

/** Returns a SourcePos that encompasses the extent of both of the given
    extents. */
SourcePos Union(const SourcePos &p1, const SourcePos &p2);


/// ABIArgInfo - Helper class to encapsulate information about how a
/// specific C type should be passed to or returned from a function.
class ABIArgInfo {
public:
	enum Kind : uint8_t {
		/// Direct - Pass the argument directly using the normal converted LLVM
		/// type, or by coercing to another specified type stored in
		/// 'CoerceToType').  If an offset is specified (in UIntData), then the
		/// argument passed is offset by some number of bytes in the memory
		/// representation. A dummy argument is emitted before the real argument
		/// if the specified type stored in "PaddingType" is not zero.
		Direct,

		/// Extend - Valid only for integer argument types. Same as 'direct'
		/// but also emit a zero/sign extension attribute.
		Extend,

		/// Indirect - Pass the argument indirectly via a hidden pointer
		/// with the specified alignment (0 indicates default alignment).
		Indirect,

		/// Ignore - Ignore the argument (treat as void). Useful for void and
		/// empty structs.
		Ignore,

		/// Expand - Only valid for aggregate argument types. The structure should
		/// be expanded into consecutive arguments for its constituent fields.
		/// Currently expand is only allowed on structures whose fields
		/// are all scalar types or are themselves expandable types.
		Expand,

		/// CoerceAndExpand - Only valid for aggregate argument types. The
		/// structure should be expanded into consecutive arguments corresponding
		/// to the non-array elements of the type stored in CoerceToType.
		/// Array elements in the type are assumed to be padding and skipped.
		CoerceAndExpand,

		/// InAlloca - Pass the argument directly using the LLVM inalloca attribute.
		/// This is similar to indirect with byval, except it only applies to
		/// arguments stored in memory and forbids any implicit copies.  When
		/// applied to a return type, it means the value is returned indirectly via
		/// an implicit sret parameter stored in the argument struct.
		InAlloca,
		KindFirst = Direct,
		KindLast = InAlloca
	};

private:
	llvm::Type *TypeData; // canHaveCoerceToType()
	union {
		llvm::Type *PaddingType; // canHavePaddingType()
		llvm::Type *UnpaddedCoerceAndExpandType; // isCoerceAndExpand()
	};
	union {
		unsigned DirectOffset;     // isDirect() || isExtend()
		unsigned IndirectAlign;    // isIndirect()
		unsigned AllocaFieldIndex; // isInAlloca()
	};
	Kind TheKind;
	bool PaddingInReg : 1;
	bool InAllocaSRet : 1;    // isInAlloca()
	bool InAllocaIndirect : 1;// isInAlloca()
	bool IndirectByVal : 1;   // isIndirect()
	bool IndirectRealign : 1; // isIndirect()
	bool SRetAfterThis : 1;   // isIndirect()
	bool InReg : 1;           // isDirect() || isExtend() || isIndirect()
	bool CanBeFlattened : 1;   // isDirect()
	bool SignExt : 1;         // isExtend()

	bool canHavePaddingType() const {
		return isDirect() || isExtend() || isIndirect() || isExpand();
	}
	void setPaddingType(llvm::Type *T) {
		//assert(canHavePaddingType());
		PaddingType = T;
	}

	void setUnpaddedCoerceToType(llvm::Type *T) {
		//assert(isCoerceAndExpand());
		UnpaddedCoerceAndExpandType = T;
	}

public:
	ABIArgInfo(Kind K = Direct)
		: TypeData(nullptr), PaddingType(nullptr), DirectOffset(0), TheKind(K),
		PaddingInReg(false), InAllocaSRet(false), InAllocaIndirect(false),
		IndirectByVal(false), IndirectRealign(false), SRetAfterThis(false),
		InReg(false), CanBeFlattened(false), SignExt(false) {}

	static ABIArgInfo getDirect(llvm::Type *T = nullptr, unsigned Offset = 0,
		llvm::Type *Padding = nullptr,
		bool CanBeFlattened = true) {
		auto AI = ABIArgInfo(Direct);
		AI.setCoerceToType(T);
		AI.setPaddingType(Padding);
		AI.setDirectOffset(Offset);
		AI.setCanBeFlattened(CanBeFlattened);
		return AI;
	}
	static ABIArgInfo getDirectInReg(llvm::Type *T = nullptr) {
		auto AI = getDirect(T);
		AI.setInReg(true);
		return AI;
	}

	/*static ABIArgInfo getSignExtend(llvm::Type Ty, llvm::Type *T = nullptr) {
		//assert(Ty->isIntegralOrEnumerationType() && "Unexpected QualType");
		auto AI = ABIArgInfo(Extend);
		AI.setCoerceToType(T);
		AI.setPaddingType(nullptr);
		AI.setDirectOffset(0);
		AI.setSignExt(true);
		return AI;
	}

	static ABIArgInfo getZeroExtend(llvm::Type Ty, llvm::Type *T = nullptr) {
		//assert(Ty->isIntegralOrEnumerationType() && "Unexpected QualType");
		auto AI = ABIArgInfo(Extend);
		AI.setCoerceToType(T);
		AI.setPaddingType(nullptr);
		AI.setDirectOffset(0);
		AI.setSignExt(false);
		return AI;
	}

	// ABIArgInfo will record the argument as being extended based on the sign
	// of its type.
	static ABIArgInfo getExtend(llvm::Type Ty, llvm::Type *T = nullptr) {
		//assert(Ty->isIntegralOrEnumerationType() && "Unexpected QualType");
		//if (Ty->hasSignedIntegerRepresentation())
		//	return getSignExtend(Ty, T);
		return getZeroExtend(Ty, T);
	}

	static ABIArgInfo getExtendInReg(llvm::Type Ty, llvm::Type *T = nullptr) {
		auto AI = getExtend(Ty, T);
		AI.setInReg(true);
		return AI;
	}*/
	/*
	static ABIArgInfo getSignExtend(QualType Ty, llvm::Type *T = nullptr) {
		assert(Ty->isIntegralOrEnumerationType() && "Unexpected QualType");
		auto AI = ABIArgInfo(Extend);
		AI.setCoerceToType(T);
		AI.setPaddingType(nullptr);
		AI.setDirectOffset(0);
		AI.setSignExt(true);
		return AI;
	}

	static ABIArgInfo getZeroExtend(QualType Ty, llvm::Type *T = nullptr) {
		assert(Ty->isIntegralOrEnumerationType() && "Unexpected QualType");
		auto AI = ABIArgInfo(Extend);
		AI.setCoerceToType(T);
		AI.setPaddingType(nullptr);
		AI.setDirectOffset(0);
		AI.setSignExt(false);
		return AI;
	}

	// ABIArgInfo will record the argument as being extended based on the sign
	// of its type.
	static ABIArgInfo getExtend(QualType Ty, llvm::Type *T = nullptr) {
		assert(Ty->isIntegralOrEnumerationType() && "Unexpected QualType");
		if (Ty->hasSignedIntegerRepresentation())
			return getSignExtend(Ty, T);
		return getZeroExtend(Ty, T);
	}

	static ABIArgInfo getExtendInReg(QualType Ty, llvm::Type *T = nullptr) {
		auto AI = getExtend(Ty, T);
		AI.setInReg(true);
		return AI;
	}*/
	static ABIArgInfo getIgnore() {
		return ABIArgInfo(Ignore);
	}
	static ABIArgInfo getIndirect(unsigned int Alignment, bool ByVal = true,
		bool Realign = false,
		llvm::Type *Padding = nullptr) {
		auto AI = ABIArgInfo(Indirect);
		//AI.setIndirectAlign(Alignment);
		AI.setIndirectByVal(ByVal);
		AI.setIndirectRealign(Realign);
		AI.setSRetAfterThis(false);
		AI.setPaddingType(Padding);
		return AI;
	}
	static ABIArgInfo getIndirectInReg(unsigned int Alignment, bool ByVal = true,
		bool Realign = false) {
		auto AI = getIndirect(Alignment, ByVal, Realign);
		AI.setInReg(true);
		return AI;
	}
	/*static ABIArgInfo getIndirect(CharUnits Alignment, bool ByVal = true,
		bool Realign = false,
		llvm::Type *Padding = nullptr) {
		auto AI = ABIArgInfo(Indirect);
		AI.setIndirectAlign(Alignment);
		AI.setIndirectByVal(ByVal);
		AI.setIndirectRealign(Realign);
		AI.setSRetAfterThis(false);
		AI.setPaddingType(Padding);
		return AI;
	}
	static ABIArgInfo getIndirectInReg(CharUnits Alignment, bool ByVal = true,
		bool Realign = false) {
		auto AI = getIndirect(Alignment, ByVal, Realign);
		AI.setInReg(true);
		return AI;
	}*/
	static ABIArgInfo getInAlloca(unsigned FieldIndex, bool Indirect = false) {
		auto AI = ABIArgInfo(InAlloca);
		AI.setInAllocaFieldIndex(FieldIndex);
		AI.setInAllocaIndirect(Indirect);
		return AI;
	}
	static ABIArgInfo getExpand() {
		auto AI = ABIArgInfo(Expand);
		AI.setPaddingType(nullptr);
		return AI;
	}
	static ABIArgInfo getExpandWithPadding(bool PaddingInReg,
		llvm::Type *Padding) {
		auto AI = getExpand();
		AI.setPaddingInReg(PaddingInReg);
		AI.setPaddingType(Padding);
		return AI;
	}

	/// \param unpaddedCoerceToType The coerce-to type with padding elements
	///   removed, canonicalized to a single element if it would otherwise
	///   have exactly one element.
	/*static ABIArgInfo getCoerceAndExpand(llvm::StructType *coerceToType,
		llvm::Type *unpaddedCoerceToType) {
		auto AI = ABIArgInfo(CoerceAndExpand);
		AI.setCoerceToType(coerceToType);
		AI.setUnpaddedCoerceToType(unpaddedCoerceToType);
		return AI;
	}

	static bool isPaddingForCoerceAndExpand(llvm::Type *eltType) {
		if (eltType->isArrayTy()) {
			//assert(eltType->getArrayElementType()->isIntegerTy(8));
			return true;
		}
		else {
			return false;
		}
	}
    */
	Kind getKind() const { return TheKind; }
	bool isDirect() const { return TheKind == Direct; }
	bool isInAlloca() const { return TheKind == InAlloca; }
	bool isExtend() const { return TheKind == Extend; }
	bool isIgnore() const { return TheKind == Ignore; }
	bool isIndirect() const { return TheKind == Indirect; }
	bool isExpand() const { return TheKind == Expand; }
	bool isCoerceAndExpand() const { return TheKind == CoerceAndExpand; }

	bool canHaveCoerceToType() const {
		return isDirect() || isExtend() || isCoerceAndExpand();
	}

	// Direct/Extend accessors
	unsigned getDirectOffset() const {
		//assert((isDirect() || isExtend()) && "Not a direct or extend kind");
		return DirectOffset;
	}
	void setDirectOffset(unsigned Offset) {
		//assert((isDirect() || isExtend()) && "Not a direct or extend kind");
		DirectOffset = Offset;
	}

	bool isSignExt() const {
		//assert(isExtend() && "Invalid kind!");
		return SignExt;
	}
	void setSignExt(bool SExt) {
		//assert(isExtend() && "Invalid kind!");
		SignExt = SExt;
	}

	llvm::Type *getPaddingType() const {
		return (canHavePaddingType() ? PaddingType : nullptr);
	}

	bool getPaddingInReg() const {
		return PaddingInReg;
	}
	void setPaddingInReg(bool PIR) {
		PaddingInReg = PIR;
	}

	llvm::Type *getCoerceToType() const {
		//assert(canHaveCoerceToType() && "Invalid kind!");
		return TypeData;
	}

	void setCoerceToType(llvm::Type *T) {
		//assert(canHaveCoerceToType() && "Invalid kind!");
		TypeData = T;
	}

	/*llvm::StructType *getCoerceAndExpandType() const {
		//assert(isCoerceAndExpand());
		return llvm::cast<llvm::StructType>(TypeData);
	}*/

	llvm::Type *getUnpaddedCoerceAndExpandType() const {
		//assert(isCoerceAndExpand());
		return UnpaddedCoerceAndExpandType;
	}

	llvm::ArrayRef<llvm::Type *>getCoerceAndExpandTypeSequence() const {
		//assert(isCoerceAndExpand());
		if (auto structTy =
			llvm::dyn_cast<llvm::StructType>(UnpaddedCoerceAndExpandType)) {
			return structTy->elements();
		}
		else {
			return llvm::makeArrayRef(&UnpaddedCoerceAndExpandType, 1);
		}
	}

	bool getInReg() const {
		//assert((isDirect() || isExtend() || isIndirect()) && "Invalid kind!");
		return InReg;
	}

	void setInReg(bool IR) {
		//assert((isDirect() || isExtend() || isIndirect()) && "Invalid kind!");
		InReg = IR;
	}

	// Indirect accessors
	/*CharUnits getIndirectAlign() const {
		//assert(isIndirect() && "Invalid kind!");
		return CharUnits::fromQuantity(IndirectAlign);
	}
	void setIndirectAlign(CharUnits IA) {
		//assert(isIndirect() && "Invalid kind!");
		IndirectAlign = IA.getQuantity();
	}*/

	bool getIndirectByVal() const {
		//assert(isIndirect() && "Invalid kind!");
		return IndirectByVal;
	}
	void setIndirectByVal(bool IBV) {
		//assert(isIndirect() && "Invalid kind!");
		IndirectByVal = IBV;
	}

	bool getIndirectRealign() const {
		//assert(isIndirect() && "Invalid kind!");
		return IndirectRealign;
	}
	void setIndirectRealign(bool IR) {
		//assert(isIndirect() && "Invalid kind!");
		IndirectRealign = IR;
	}

	bool isSRetAfterThis() const {
		//assert(isIndirect() && "Invalid kind!");
		return SRetAfterThis;
	}
	void setSRetAfterThis(bool AfterThis) {
		//assert(isIndirect() && "Invalid kind!");
		SRetAfterThis = AfterThis;
	}

	unsigned getInAllocaFieldIndex() const {
		//assert(isInAlloca() && "Invalid kind!");
		return AllocaFieldIndex;
	}
	void setInAllocaFieldIndex(unsigned FieldIndex) {
		//assert(isInAlloca() && "Invalid kind!");
		AllocaFieldIndex = FieldIndex;
	}

	unsigned getInAllocaIndirect() const {
		assert(isInAlloca() && "Invalid kind!");
		return InAllocaIndirect;
	}
	void setInAllocaIndirect(bool Indirect) {
		//assert(isInAlloca() && "Invalid kind!");
		InAllocaIndirect = Indirect;
	}

	/// Return true if this field of an inalloca struct should be returned
	/// to implement a struct return calling convention.
	bool getInAllocaSRet() const {
		//assert(isInAlloca() && "Invalid kind!");
		return InAllocaSRet;
	}

	void setInAllocaSRet(bool SRet) {
		//assert(isInAlloca() && "Invalid kind!");
		InAllocaSRet = SRet;
	}

	bool getCanBeFlattened() const {
		//assert(isDirect() && "Invalid kind!");
		return CanBeFlattened;
	}

	void setCanBeFlattened(bool Flatten) {
		//assert(isDirect() && "Invalid kind!");
		CanBeFlattened = Flatten;
	}

    void print() {
        switch (TheKind) {
        case ABIArgInfo::Kind::Direct:
            printf("\n Direct \n");
            if (getInReg()) {
                // llvmFunctionType->setpara
                // argIter->addAttr(llvm::Attribute::InReg);
                printf("\n Inreg \n");
            } else {
                printf("\n No Inreg \n");
            }
            break;
        case ABIArgInfo::Kind::Indirect:
            printf("\n Indirect \n");
            if (getInReg()) {
                printf("\n Inreg \n");
            } else {
                printf("\n No Inreg \n");
            }
            break;
        case ABIArgInfo::Kind::Expand:
            printf("\n Expand \n");
            if (getInReg()) {
                printf("\n Inreg \n");
            } else {
                printf("\n No Inreg \n");
            }
            break;
        case ABIArgInfo::Kind::InAlloca:
            printf("\n InAlloca \n");
            if (getInReg()) {
                printf("\n Inreg \n");
            }
            break;
        case ABIArgInfo::Kind::Ignore:
            printf("\n Ignore \n");
            if (getInReg()) {
                printf("\n Inreg \n");
            } else {
                printf("\n No Inreg \n");
            }
            break;
        default:
            printf("\n CRASHHHHH 1 \n");
            assert(0);
        }

    }
};

/** @brief Structure that defines a compilation target

    This structure defines a compilation target for the ispc compiler.
*/
class Target {
  public:
    /** Enumerator giving the instruction sets that the compiler can
        target.  These should be ordered from "worse" to "better" in that
        if a processor supports multiple target ISAs, then the most
        flexible/performant of them will apear last in the enumerant.  Note
        also that __best_available_isa() needs to be updated if ISAs are
        added or the enumerant values are reordered.  */
    enum ISA {
        SSE2 = 0,
        SSE4 = 1,
        AVX = 2,
        // Not supported anymore. Use either AVX or AVX2.
        // AVX11 = 3,
        AVX2 = 3,
        KNL_AVX512 = 4,
        SKX_AVX512 = 5,
#ifdef ISPC_ARM_ENABLED
        NEON,
#endif
#ifdef ISPC_WASM_ENABLED
        WASM,
#endif
        NUM_ISAS
    };

    /** Initializes the given Target pointer for a target of the given
        name, if the name is a known target.  Returns true if the
        target was initialized and false if the name is unknown. */
    Target(Arch arch, const char *cpu, ISPCTarget isa, bool pic, bool printTarget);

    /** Returns a comma-delimited string giving the names of the currently
        supported CPUs. */
    static std::string SupportedCPUs();

    /** Returns a triple string specifying the target architecture, vendor,
        and environment. */
    std::string GetTripleString() const;

    /** Returns the LLVM TargetMachine object corresponding to this
        target. */
    llvm::TargetMachine *GetTargetMachine() const { return m_targetMachine; }

    /** Convert ISA enum to string */
    static const char *ISAToString(Target::ISA isa);

    /** Returns a string like "avx" encoding the target. Good for mangling. */
    const char *GetISAString() const;

    /** Convert ISA enum to string */
    static const char *ISAToTargetString(Target::ISA isa);

    /** Returns a string like "avx2-i32x8" encoding the target.
        This may be used for Target initialization. */
    const char *GetISATargetString() const;

    /** Returns the size of the given type */
    llvm::Value *SizeOf(llvm::Type *type, llvm::BasicBlock *insertAtEnd);

    /** Given a structure type and an element number in the structure,
        returns a value corresponding to the number of bytes from the start
        of the structure where the element is located. */
    llvm::Value *StructOffset(llvm::Type *type, int element, llvm::BasicBlock *insertAtEnd);

    /** Mark LLVM function with target specific attribute, if required. */
    void markFuncWithTargetAttr(llvm::Function *func);

    /** Set LLVM function with Calling Convention. */
    void markFuncWithCallingConv(llvm::Function *func);

    void setInRegForFunction(llvm::Function *function, std::vector<ABIArgInfo> &argInfo);

    //void computeInfo(llvm::Function *func, std::vector<ABIArgInfo> &argInfo);
    void computeInfo(llvm::FunctionType *fType, std::vector<ABIArgInfo> &argInfo);

    const llvm::Target *getTarget() const { return m_target; }

    // Note the same name of method for 3.1 and 3.2+, this allows
    // to reduce number ifdefs on client side.
    const llvm::DataLayout *getDataLayout() const { return m_dataLayout; }

    /** Reports if Target object has valid state. */
    bool isValid() const { return m_valid; }

    ISPCTarget getISPCTarget() const { return m_ispc_target; }

    ISA getISA() const { return m_isa; }

    Arch getArch() const { return m_arch; }

    bool is32Bit() const { return m_is32Bit; }

    std::string getCPU() const { return m_cpu; }

    int getNativeVectorWidth() const { return m_nativeVectorWidth; }

    int getNativeVectorAlignment() const { return m_nativeVectorAlignment; }

    int getDataTypeWidth() const { return m_dataTypeWidth; }

    int getVectorWidth() const { return m_vectorWidth; }

    bool getGeneratePIC() const { return m_generatePIC; }

    bool getMaskingIsFree() const { return m_maskingIsFree; }

    int getMaskBitCount() const { return m_maskBitCount; }

    bool hasHalf() const { return m_hasHalf; }

    bool hasRand() const { return m_hasRand; }

    bool hasGather() const { return m_hasGather; }

    bool hasScatter() const { return m_hasScatter; }

    bool hasTranscendentals() const { return m_hasTranscendentals; }

    bool hasTrigonometry() const { return m_hasTrigonometry; }

    bool hasRsqrtd() const { return m_hasRsqrtd; }

    bool hasRcpd() const { return m_hasRcpd; }

    bool hasVecPrefetch() const { return m_hasVecPrefetch; }

  private:
    /** llvm Target object representing this target. */
    const llvm::Target *m_target;

    /** llvm TargetMachine.
        Note that it's not destroyed during Target destruction, as
        Module::CompileAndOutput() uses TargetMachines after Target is destroyed.
        This needs to be changed. */
    llvm::TargetMachine *m_targetMachine;
    llvm::DataLayout *m_dataLayout;

    /** flag to report invalid state after construction
        (due to bad parameters passed to constructor). */
    bool m_valid;

    /** ISPC target being used */
    ISPCTarget m_ispc_target;

    /** Instruction set being compiled to. */
    ISA m_isa;

    /** Target system architecture.  (e.g. "x86-64", "x86"). */
    Arch m_arch;

    /** Is the target architecture 32 or 64 bit */
    bool m_is32Bit;

    /** Target CPU. (e.g. "corei7", "corei7-avx", ..) */
    std::string m_cpu;

    /** Target-specific attribute string to pass along to the LLVM backend */
    std::string m_attributes;

    /** Target-specific function attributes */
    std::vector<std::pair<std::string, std::string>> m_funcAttributes;

    /** Target-specific LLVM attribute, which has to be attached to every
        function to ensure that it is generated for correct target architecture.
        This is requirement was introduced in LLVM 3.3 */
    llvm::AttrBuilder *m_tf_attributes;

    /** Native vector width of the vector instruction set.  Note that this
        value is directly derived from the ISA being used (e.g. it's 4 for
        SSE, 8 for AVX, etc.) */
    int m_nativeVectorWidth;

    /** Native vector alignment in bytes. Theoretically this may be derived
        from the vector size, but it's better to manage directly the alignement.
        It allows easier experimenting and better fine tuning for particular
        platform. This information is primatily used when
        --opt=force-aligned-memory is used. */
    int m_nativeVectorAlignment;

    /** Data type width in bits. Typically it's 32, but could be 8, 16 or 64. */
    int m_dataTypeWidth;

    /** Actual vector width currently being compiled to.  This may be an
        integer multiple of the native vector width, for example if we're
        "doubling up" and compiling 8-wide on a 4-wide SSE system. */
    int m_vectorWidth;

    /** Indicates whether position independent code should be generated. */
    bool m_generatePIC;

    /** Is there overhead associated with masking on the target
        architecture; e.g. there is on SSE, due to extra blends and the
        like, but there isn't with an ISA that supports masking
        natively. */
    bool m_maskingIsFree;

    /** How many bits are used to store each element of the mask: e.g. this
        is 32 on SSE/AVX, since that matches the HW better. */
    int m_maskBitCount;

    /** Indicates whether the target has native support for float/half
        conversions. */
    bool m_hasHalf;

    /** Indicates whether there is an ISA random number instruction. */
    bool m_hasRand;

    /** Indicates whether the target has a native gather instruction */
    bool m_hasGather;

    /** Indicates whether the target has a native scatter instruction */
    bool m_hasScatter;

    /** Indicates whether the target has support for transcendentals (beyond
        sqrt, which we assume that all of them handle). */
    bool m_hasTranscendentals;

    /** Indicates whether the target has ISA support for trigonometry */
    bool m_hasTrigonometry;

    /** Indicates whether there is an ISA double precision rsqrt. */
    bool m_hasRsqrtd;

    /** Indicates whether there is an ISA double precision rcp. */
    bool m_hasRcpd;

    /** Indicates whether the target has hardware instruction for vector prefetch. */
    bool m_hasVecPrefetch;
};

/** @brief Structure that collects optimization options

    This structure collects all of the options related to optimization of
    generated code.
*/
struct Opt {
    Opt();

    /** Optimization level.  Currently, the only valid values are 0,
        indicating essentially no optimization, and 1, indicating as much
        optimization as possible. */
    int level;

    /** Indicates whether "fast and loose" numerically unsafe optimizations
        should be performed.  This is false by default. */
    bool fastMath;

    /** Indicates whether an vector load should be issued for masked loads
        on platforms that don't have a native masked vector load.  (This may
        lead to accessing memory up to programCount-1 elements past the end of
        arrays, so is unsafe in general.) */
    bool fastMaskedVload;

    /** Indicates when loops should be unrolled (when doing so seems like
        it will make sense. */
    bool unrollLoops;

    /** Indicates if addressing math will be done with 32-bit math, even on
        64-bit systems.  (This is generally noticably more efficient,
        though at the cost of addressing >2GB).
     */
    bool force32BitAddressing;

    /** Indicates whether Assert() statements should be ignored (for
        performance in the generated code). */
    bool disableAsserts;

    /** Indicates whether FMA instructions should be disabled (on targets
        that support them). */
    bool disableFMA;

    /** Always generate aligned vector load/store instructions; this
        implies a guarantee that all dynamic access through pointers that
        becomes a vector load/store will be a cache-aligned sequence of
        locations. */
    bool forceAlignedMemory;

    /** If enabled, disables the various optimizations that kick in when
        the execution mask can be determined to be "all on" at compile
        time. */
    bool disableMaskAllOnOptimizations;

    /** If enabled, the various __pseudo* memory ops (gather/scatter,
        masked load/store) are left in their __pseudo* form, for better
        understanding of the structure of generated code when reading
        it. */
    bool disableHandlePseudoMemoryOps;

    /** On targets that don't have a masked store instruction but do have a
        blending instruction, by default, we simulate masked stores by
        loading the old value, blending, and storing the result.  This can
        potentially be unsafe in multi-threaded code, in that it writes to
        locations that aren't supposed to be written to.  Setting this
        value to true disables this work-around, and instead implements
        masked stores by 'scalarizing' them, so that we iterate over the
        ISIMD lanes and do a scalar write for the ones that are running. */
    bool disableBlendedMaskedStores;

    /** Disables the 'coherent control flow' constructs in the
        language. (e.g. this causes "cif" statements to be demoted to "if"
        statements.)  This is likely only useful for measuring the impact
        of coherent control flow. */
    bool disableCoherentControlFlow;

    /** Disables uniform control flow optimizations (e.g. this changes an
        "if" statement with a uniform condition to have a varying
        condition).  This is likely only useful for measuring the impact of
        uniform control flow. */
    bool disableUniformControlFlow;

    /** Disables the backend optimizations related to gather/scatter
        (e.g. transforming gather from sequential locations to an unaligned
        load, etc.)  This is likely only useful for measuring the impact of
        these optimizations. */
    bool disableGatherScatterOptimizations;

    /** Disables the optimization that demotes masked stores to regular
        stores when the store is happening at the same control flow level
        where the variable was declared.  This is likely only useful for
        measuring the impact of this optimization. */
    bool disableMaskedStoreToStore;

    /** Disables the optimization that detects when the execution mask is
        all on and emits code for gathers and scatters that doesn't loop
        over the SIMD lanes but just does the scalar loads and stores
        directly. */
    bool disableGatherScatterFlattening;

    /** Disables the optimizations that detect when arrays are being
        indexed with 'uniform' values and issue scalar loads/stores rather
        than gathers/scatters.  This is likely only useful for measuring
        the impact of this optimization. */
    bool disableUniformMemoryOptimizations;

    /** Disables optimizations that coalesce incoherent scalar memory
        access from gathers into wider vector operations, when possible. */
    bool disableCoalescing;

    /** Disable using zmm registers for avx512 target in favour of ymm.
        Affects only >= 512 bit wide targets and only if avx512vl is available */
    bool disableZMM;
};

/** @brief This structure collects together a number of global variables.

    This structure collects a number of global variables that mostly
    represent parameter settings for this compilation run.  In particular,
    none of these values should change after compilation befins; their
    values are all set during command-line argument processing or very
    early during the compiler's execution, before any files are parsed.
  */
struct Globals {
    Globals();

    /** TargetRegistry holding all stdlib bitcode. */
    TargetLibRegistry *target_registry;

    /** Optimization option settings */
    Opt opt;

    /** Compilation target information */
    Target *target;

    /** Target OS */
    TargetOS target_os;

    /** Function Calling Convention */
    CallingConv calling_conv;

    ABIInfo abiInfo;

    /** There are a number of math libraries that can be used for
        transcendentals and the like during program compilation. */
    enum MathLib { Math_ISPC, Math_ISPCFast, Math_SVML, Math_System };
    MathLib mathLib;

    /** Optimization level to be specified while creating TargetMachine. */
    enum CodegenOptLevel { None, Aggressive };
    CodegenOptLevel codegenOptLevel;

    /** Records whether the ispc standard library should be made available
        to the program during compilations. (Default is true.) */
    bool includeStdlib;

    /** Indicates whether the C pre-processor should be run over the
        program source before compiling it.  (Default is true.) */
    bool runCPP;

    /** When \c true, voluminous debugging output will be printed during
        ispc's execution. */
    bool debugPrint;

    /** When \c true, target ISA will be printed during ispc's execution. */
    bool printTarget;

    /** When \c true, LLVM won't omit frame pointer. */
    bool NoOmitFramePointer;

    /** Indicates which stages of optimization we want to dump. */
    std::set<int> debug_stages;

    /** Whether to dump IR to file. */
    bool dumpFile;

    /** Indicates after which optimization we want to generate
        DebugIR information. */
    int debugIR;

    /** Indicates which phases of optimization we want to switch off. */
    std::set<int> off_stages;

    /** Indicates whether all warning messages should be surpressed. */
    bool disableWarnings;

    /** Indicates whether warnings should be issued as errors. */
    bool warningsAsErrors;

    /** Indicates whether line wrapping of error messages to the terminal
        width should be disabled. */
    bool disableLineWrap;

    /** Indicates whether additional warnings should be issued about
        possible performance pitfalls. */
    bool emitPerfWarnings;

    /** Indicates whether all printed output should be surpressed. */
    bool quiet;

    /** Always use ANSI escape sequences to colorize warning and error
        messages, even if piping output to a file, etc. */
    bool forceColoredOutput;

    /** Indicates whether calls should be emitted in the program to an
        externally-defined program instrumentation function. (See the
        "Instrumenting your ispc programs" section in the user's
        manual.) */
    bool emitInstrumentation;

    bool noPragmaOnce;

    /** Indicates whether ispc should generate debugging symbols for the
        program in its output. */
    bool generateDebuggingSymbols;

    /** Require generation of DWARF of certain version (2, 3, 4). For
        default version, this field is set to 0. */
    // Hint: to verify dwarf version in the object file, run on Linux:
    // readelf --debug-dump=info object.o | grep -A 2 'Compilation Unit @'
    // on Mac:
    // xcrun dwarfdump -r0 object.o
    int generateDWARFVersion;

    /** If true, function names are mangled by appending the target ISA and
        vector width to them. */
    bool mangleFunctionsWithTarget;

    /** If enabled, the lexer will randomly replace some tokens returned
        with other tokens, in order to test error condition handling in the
        compiler. */
    bool enableFuzzTest;

    /** Seed for random number generator used for fuzz testing. */
    int fuzzTestSeed;

    /** Global LLVMContext object */
    llvm::LLVMContext *ctx;

    /** Current working directory when the ispc compiler starts
        execution. */
    char currentDirectory[1024];

    /** Arguments to pass along to the C pre-processor, if it is run on the
        program before compilation. */
    std::vector<std::string> cppArgs;

    /** Additional user-provided directories to search when processing
        #include directives in the preprocessor. */
    std::vector<std::string> includePath;

    /** Indicates that alignment in memory allocation routines should be
        forced to have given value. -1 value means natural alignment for the platforms. */
    int forceAlignment;

    /** When true, flag non-static functions with dllexport attribute on Windows. */
    bool dllExport;

    /** Lines for which warnings are turned off. */
    std::map<std::pair<int, std::string>, bool> turnOffWarnings;

    /* If true, we are compiling for more than one target. */
    bool isMultiTargetCompilation;

    /* Number of errors to show in ISPC. */
    int errorLimit;
};

enum {
    COST_ASSIGN = 1,
    COST_COMPLEX_ARITH_OP = 4,
    COST_DELETE = 32,
    COST_DEREF = 4,
    COST_FUNCALL = 4,
    COST_FUNPTR_UNIFORM = 12,
    COST_FUNPTR_VARYING = 24,
    COST_GATHER = 8,
    COST_GOTO = 4,
    COST_LOAD = 2,
    COST_NEW = 32,
    COST_BREAK_CONTINUE = 3,
    COST_RETURN = 4,
    COST_SELECT = 4,
    COST_SIMPLE_ARITH_LOGIC_OP = 1,
    COST_SYNC = 32,
    COST_TASK_LAUNCH = 32,
    COST_TYPECAST_COMPLEX = 4,
    COST_TYPECAST_SIMPLE = 1,
    COST_UNIFORM_IF = 2,
    COST_VARYING_IF = 3,
    COST_UNIFORM_LOOP = 4,
    COST_VARYING_LOOP = 6,
    COST_UNIFORM_SWITCH = 4,
    COST_VARYING_SWITCH = 12,
    COST_ASSERT = 8,

    CHECK_MASK_AT_FUNCTION_START_COST = 16,
    PREDICATE_SAFE_IF_STATEMENT_COST = 6,
};

extern Globals *g;
extern Module *m;
