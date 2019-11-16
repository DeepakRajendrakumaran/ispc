/*
  Copyright (c) 2019, Intel Corporation
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

#define EPSILON 0.01f

#include "structABIRules_ispc.h"
#include <iostream>

/* This examples tests returning structs from ISPC to CPP and CPP to ISPC.
 * Each scenario below tests a different structs. The structs being tested were
 * chosen based on x86-64 ABI rules for returning from functions.*/

void printResult(bool result) {
    std::cout << "\n CPP SIDE RESULT : ";
    if (result) {
        std::cout << "    PASSED" << std::endl;
    } else {
        std::cout << "    FAILED" << std::endl;
    }
}

/* Methods to check if the struct passed back has the correct values */
void checkGetSingleIntegerStructFromIspc(ispc::singleInteger struc, int int1) { printResult(struc.a == int1); }

void checkGetMulIntegerStructFromIspc(ispc::mulInteger struc, int int1, int int2, int int3) {
    printResult(struc.a == int1 && struc.b == int2 && struc.c == int3);
}

void checkGetSingleFloatStructFromIspc(ispc::singleFloat struc, float f1) { printResult(struc.a == f1); }

void checkGetMulFloatStructFromIspc(ispc::mulFloat struc, float f1, float f2, float f3) {
    printResult(struc.a == f1 && struc.b == f2 && struc.c == f3);
}

void checkGetMulNonRegSizeStructFromIspc(ispc::mulNonRegSize struc, unsigned char int8_1, unsigned char int8_2,
                                         unsigned char int8_3) {
    printResult(struc.a == int8_1 && struc.b == int8_2 && struc.c == int8_3);
}

void checkGetEnumElemStructFromIspc(ispc::enumElem struc, unsigned char int8_1, ispc::enumforStruct enum1) {
    printResult(struc.a == int8_1 && struc.b == enum1);
}

void checkWithDoubleElemStructFromIspc(ispc::withDoubleElem struc, double d1) { printResult(struc.a == d1); }

void checkWithPointerElemStructFromIspc(ispc::withPointerElem struc, int int1, ispc::structForStruct *struct1) {
    printResult(struc.a == int1 && struc.b->x == struct1->x && (struc.b)->y == struct1->y);
}

void checkWithArrayElemStructFromIspc(ispc::withArrayElem struc, float f_arr[3]) {
    printResult(struc.a[0] == f_arr[0] && struc.a[1] == f_arr[1] && struc.a[2] == f_arr[2]);
}

void checkWithNestedStructElemStructFromIspc(ispc::withNestedStruct struc, int int1, ispc::structForStruct *struct1,
                                             float f1) {
    printResult(struc.a == int1 && struc.b.x == struct1->x && struc.b.y == struct1->y && struc.c == f1);
}

void checkWithMixedArrayElemStructFromIspc(ispc::withMixedElemsArray struc, int int1, float f_arr[3]) {
    printResult(struc.a == int1 && struc.f[0] == f_arr[0] && struc.f[1] == f_arr[1] && struc.f[2] == f_arr[2]);
}

void checkPassAsArgStructFromIspc(ispc::passAsArg struc, int int1, ispc::structForStruct *struct1, double d1) {
    printResult(struc.a == int1 && struc.b.x == struct1->x && struc.b.y == struct1->y && *struc.c == d1);
}

/* Functions called from ISPC side which returns struct from CPP to ISPC*/
extern "C" ispc::singleInteger getSingleIntegerStructFromCpp(int int1) {
    ispc::singleInteger struc;
    struc.a = int1;
    return struc;
}

extern "C" ispc::mulInteger getMulIntegerStructFromCpp(int int1, int int2, int int3) {
    ispc::mulInteger struc;
    struc.a = int1;
    struc.b = int2;
    struc.c = int3;
    return struc;
}

extern "C" ispc::singleFloat getSingleFloatStructFromCpp(float f1) {
    ispc::singleFloat struc;
    struc.a = f1;
    return struc;
}

extern "C" ispc::mulFloat getMulFloatStructFromCpp(float f1, float f2, float f3) {
    ispc::mulFloat struc;
    struc.a = f1;
    struc.b = f2;
    struc.c = f3;
    return struc;
}

extern "C" ispc::mulNonRegSize getMulNonRegSizeStructFromCpp(unsigned char int8_1, unsigned char int8_2,
                                                             unsigned char int8_3) {
    ispc::mulNonRegSize struc;
    struc.a = int8_1;
    struc.b = int8_2;
    struc.c = int8_3;
    return struc;
}

extern "C" ispc::enumElem getEnumElemStructFromCpp(unsigned char int8_1, ispc::enumforStruct enum1) {
    ispc::enumElem struc;
    struc.a = int8_1;
    struc.b = enum1;
    return struc;
}

extern "C" ispc::withDoubleElem getWithDoubleElemStructFromCpp(double d1) {
    ispc::withDoubleElem struc;
    struc.a = d1;
    return struc;
}

extern "C" ispc::withPointerElem getWithPointerElemStructFromCpp(int int1, ispc::structForStruct *struct1) {
    ispc::withPointerElem struc;
    struc.a = int1;
    struc.b = struct1;
    return struc;
}

extern "C" ispc::withArrayElem getWithArrayElemStructFromCpp(float f_arr[3]) {
    ispc::withArrayElem struc;
    struc.a[0] = f_arr[0];
    struc.a[1] = f_arr[1];
    struc.a[2] = f_arr[2];
    return struc;
}

extern "C" ispc::withNestedStruct getWithNestedStructFromCpp(int int1, ispc::structForStruct *struct1, float f1) {
    ispc::withNestedStruct struc;
    struc.a = int1;
    struc.b.x = struct1->x;
    struc.b.y = struct1->y;
    struc.c = f1;
    return struc;
}

extern "C" ispc::withMixedElemsArray getWithMixedArrayElemStructFromCpp(int int1, float f_arr[3]) {
    ispc::withMixedElemsArray struc;
    struc.a = int1;
    struc.f[0] = f_arr[0];
    struc.f[1] = f_arr[1];
    struc.f[2] = f_arr[2];
    return struc;
}

extern "C" ispc::passAsArg getPassAsArgStructFromCpp(int int1, ispc::structForStruct *struct1, double *d1) {
    ispc::passAsArg struc;
    struc.a = int1;
    struc.b.x = struct1->x;
    struc.b.y = struct1->y;
    struc.c = d1;
    return struc;
}

void runTests() {

    int int1, int2, int3;
    float f1, f2, f3;
    unsigned char int8_1, int8_2, int8_3;
    ispc::enumforStruct enum1;
    double d1;
    ispc::structForStruct *struct1 = new ispc::structForStruct;
    ;
    float f_arr[3];

    int1 = 7;
    std::cout << "\n\n\n Executing test : getSingleIntegerStructFromIspc \n";
    ispc::singleInteger s1 = ispc::getSingleIntegerStructFromIspc(int1);
    checkGetSingleIntegerStructFromIspc(s1, int1);

    int1 = 9;
    int2 = 4;
    int3 = 6;
    std::cout << "\n\n\n Executing test : getMulIntegerStructFromIspc \n";
    ispc::mulInteger s2 = ispc::getMulIntegerStructFromIspc(int1, int2, int3);
    checkGetMulIntegerStructFromIspc(s2, int1, int2, int3);

    f1 = 3.4f;
    std::cout << "\n\n\n Executing test : getSingleFloatStructFromIspc \n";
    ispc::singleFloat s3 = ispc::getSingleFloatStructFromIspc(f1);
    checkGetSingleFloatStructFromIspc(s3, f1);

    f1 = 2.1f;
    f2 = 9.8f;
    f3 = 7.3f;
    std::cout << "\n\n\n Executing test : getMulFloatStructFromIspc \n";
    ispc::mulFloat s4 = ispc::getMulFloatStructFromIspc(f1, f2, f3);
    checkGetMulFloatStructFromIspc(s4, f1, f2, f3);

    int8_1 = 3;
    int8_2 = 4;
    int8_3 = 5;
    std::cout << "\n\n\n Executing test : getMulNonRegSizeStructFromIspc \n";
    ispc::mulNonRegSize s5 = ispc::getMulNonRegSizeStructFromIspc(int8_1, int8_2, int8_3);
    checkGetMulNonRegSizeStructFromIspc(s5, int8_1, int8_2, int8_3);

    int8_1 = 7;
    std::cout << "\n\n\n Executing test : getEnumElemStructFromIspc \n";
    enum1 = ispc::enumforStruct::two;
    ispc::enumElem s6 = ispc::getEnumElemStructFromIspc(int8_1, enum1);
    checkGetEnumElemStructFromIspc(s6, int8_1, enum1);

    d1 = 8.7;
    std::cout << "\n\n\n Executing test : getWithDoubleElemStructFromIspc \n";
    ispc::withDoubleElem s7 = ispc::getWithDoubleElemStructFromIspc(d1);
    checkWithDoubleElemStructFromIspc(s7, d1);

    int1 = 7;
    struct1->x = 3.3f;
    struct1->y = 5;
    std::cout << "\n\n\n Executing test : getWithPointerElemStructFromIspc \n";
    ispc::withPointerElem s8 = ispc::getWithPointerElemStructFromIspc(int1, struct1);
    checkWithPointerElemStructFromIspc(s8, int1, struct1);

    f_arr[0] = 1.7f;
    f_arr[1] = 5.3f;
    f_arr[2] = 7.5f;
    std::cout << "\n\n\n Executing test : getWithArrayElemStructFromIspc \n";
    ispc::withArrayElem s9 = ispc::getWithArrayElemStructFromIspc(f_arr);
    checkWithArrayElemStructFromIspc(s9, f_arr);

    int1 = 7;
    struct1->x = 2.2f;
    struct1->y = 5;
    f1 = 3.3f;
    std::cout << "\n\n\n Executing test : getWithNestedStructFromIspc \n";
    ispc::withNestedStruct s10 = ispc::getWithNestedStructFromIspc(int1, struct1, f1);
    checkWithNestedStructElemStructFromIspc(s10, int1, struct1, f1);

    int1 = 9;
    f_arr[0] = 6.2f;
    f_arr[1] = 5.1f;
    f_arr[2] = 3.3f;
    std::cout << "\n\n\n Executing test : getWithMixedArrayElemStructFromIspc \n";
    ispc::withMixedElemsArray s11 = ispc::getWithMixedArrayElemStructFromIspc(int1, f_arr);
    checkWithMixedArrayElemStructFromIspc(s11, int1, f_arr);

    int1 = 3;
    struct1->x = 9.2f;
    struct1->y = 3;
    d1 = 8.5;
    std::cout << "\n\n\n Executing test : getPassAsArgStructFromIspc \n";
    ispc::passAsArg s12 = ispc::getPassAsArgStructFromIspc(int1, struct1, d1);
    checkPassAsArgStructFromIspc(s12, int1, struct1, d1);
}

int main() {

    runTests();
    return 0;
}
