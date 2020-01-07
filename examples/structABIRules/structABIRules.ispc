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

enum enumforStruct { zero, one, two, three };

struct empty {};

struct structWithEmpty {
    struct empty ee1;
    int8 x;
};

struct structForStruct {
    float x;
    int y;
};

struct singleInteger {
    int a;
};

struct mulInteger {
    int a, b, c;
};

struct singleFloat {
    float a;
};

struct mulFloat {
    float a, b, c;
};

struct mulNonRegSize {
    uint8 a, b, c;
};

struct enumElem {
    uint8 a;
    enumforStruct b;
};

struct withDoubleElem {
    double a;
};

struct withPointerElem {
    int a;
    structForStruct *b;
};

struct withArrayElem {
    float a[3];
};

struct withNestedStruct {
    int a;
    structForStruct b;
    float c;
};

struct withMixedElemsArray {
    int a;
    float f[3];
};

struct passAsArg {
    int a;
    structForStruct b;
    double *c;
};

struct nestedEmpty {
    struct empty e1;
    struct empty e2;
    int16 a;
    struct structWithEmpty b;
    int8 c, d, e;
};

extern "C" uniform singleInteger getSingleIntegerStructFromCpp(uniform int int1);
extern "C" uniform mulInteger getMulIntegerStructFromCpp(uniform int int1, uniform int int2, uniform int int3);
extern "C" uniform singleFloat getSingleFloatStructFromCpp(uniform float f1);
extern "C" uniform mulFloat getMulFloatStructFromCpp(uniform float f1, uniform float f2, uniform float f3);
extern "C" uniform mulNonRegSize getMulNonRegSizeStructFromCpp(uniform uint8 int8_1, uniform uint8 int8_2,
                                                               uniform uint8 int8_3);
extern "C" uniform enumElem getEnumElemStructFromCpp(uniform uint8 int8_1, uniform enumforStruct enum1);
extern "C" uniform withDoubleElem getWithDoubleElemStructFromCpp(uniform double d1);
extern "C" uniform withPointerElem getWithPointerElemStructFromCpp(uniform int int1,
                                                                   uniform structForStruct *uniform struct1);
extern "C" uniform withArrayElem getWithArrayElemStructFromCpp(uniform float f_arr[3]);
extern "C" uniform withNestedStruct getWithNestedStructFromCpp(uniform int int1,
                                                               uniform structForStruct *uniform struct1,
                                                               uniform float f1);
extern "C" uniform withMixedElemsArray getWithMixedArrayElemStructFromCpp(uniform int int1, uniform float f_arr[3]);
extern "C" uniform passAsArg getPassAsArgStructFromCpp(uniform int int1, uniform structForStruct *uniform struct1,
                                                       uniform double *uniform d1);
extern "C" uniform nestedEmpty getWithNestedEmptyStructFromCpp(uniform int16 int16_1,
                                                               uniform structWithEmpty *uniform struct1,
                                                               uniform int8 int8_1, uniform int8 int8_2,
                                                               uniform int8 int8_3);

/* Functions called from the CPP side which returns a struct back to CPP.
 * Within this function, we also check returning from CPP to ISPC */
export uniform singleInteger getSingleIntegerStructFromIspc(uniform int int1) {
    uniform singleInteger struc = getSingleIntegerStructFromCpp(int1);
    assert(struc.a == int1);
    return struc;
}

export uniform mulInteger getMulIntegerStructFromIspc(uniform int int1, uniform int int2, uniform int int3) {
    uniform mulInteger struc = getMulIntegerStructFromCpp(int1, int2, int3);
    assert(struc.a == int1 && struc.b == int2 && struc.c == int3);
    return struc;
}

export uniform singleFloat getSingleFloatStructFromIspc(uniform float f1) {
    uniform singleFloat struc = getSingleFloatStructFromCpp(f1);
    assert(struc.a == f1);
    return struc;
}

export uniform mulFloat getMulFloatStructFromIspc(uniform float f1, uniform float f2, uniform float f3) {
    uniform mulFloat struc = getMulFloatStructFromCpp(f1, f2, f3);
    assert(struc.a == f1 && struc.b == f2 && struc.c == f3);
    return struc;
}

export uniform mulNonRegSize getMulNonRegSizeStructFromIspc(uniform uint8 int8_1, uniform uint8 int8_2,
                                                            uniform uint8 int8_3) {
    uniform mulNonRegSize struc = getMulNonRegSizeStructFromCpp(int8_1, int8_2, int8_3);
    assert(struc.a == int8_1 && struc.b == int8_2 && struc.c == int8_3);
    return struc;
}

export uniform enumElem getEnumElemStructFromIspc(uniform uint8 int8_1, uniform enumforStruct enum1) {
    uniform enumElem struc = getEnumElemStructFromCpp(int8_1, enum1);
    assert(struc.a == int8_1 && struc.b == enum1);
    return struc;
}

export uniform withDoubleElem getWithDoubleElemStructFromIspc(uniform double d1) {
    uniform withDoubleElem struc = getWithDoubleElemStructFromCpp(d1);
    assert(struc.a == d1);
    return struc;
}

export uniform withPointerElem getWithPointerElemStructFromIspc(uniform int int1,
                                                                uniform structForStruct *uniform struct1) {
    uniform withPointerElem struc = getWithPointerElemStructFromCpp(int1, struct1);
    assert(struc.a == int1 && struc.b->x == struct1->x && (struc.b)->y == struct1->y);
    return struc;
}

export uniform withArrayElem getWithArrayElemStructFromIspc(uniform float f_arr[3]) {
    uniform withArrayElem struc = getWithArrayElemStructFromCpp(f_arr);
    assert(struc.a[0] == f_arr[0] && struc.a[1] == f_arr[1] && struc.a[2] == f_arr[2]);
    return struc;
}

export uniform withNestedStruct getWithNestedStructFromIspc(uniform int int1, uniform structForStruct *uniform struct1,
                                                            uniform float f1) {
    uniform withNestedStruct struc = getWithNestedStructFromCpp(int1, struct1, f1);
    assert(struc.a == int1 && struc.b.x == struct1->x && struc.b.y == struct1->y && struc.c == f1);
    return struc;
}

export uniform withMixedElemsArray getWithMixedArrayElemStructFromIspc(uniform int int1, uniform float f_arr[3]) {
    uniform withMixedElemsArray struc = getWithMixedArrayElemStructFromCpp(int1, f_arr);
    assert(struc.a == int1 && struc.f[0] == f_arr[0] && struc.f[1] == f_arr[1] && struc.f[2] == f_arr[2]);
    return struc;
}

export uniform passAsArg getPassAsArgStructFromIspc(uniform int int1, uniform structForStruct *uniform struct1,
                                                    uniform double d1) {
    uniform double *uniform d_ptr = uniform new uniform double;
    *d_ptr = d1;
    uniform passAsArg struc = getPassAsArgStructFromCpp(int1, struct1, d_ptr);
    assert(struc.a == int1 && struc.b.x == struct1->x && struc.b.y == struct1->y && *struc.c == d1);
    return struc;
}

export uniform nestedEmpty getWithNestedEmptyStructFromIspc(uniform int16 int16_1,
                                                            uniform structWithEmpty *uniform struct1,
                                                            uniform int8 int8_1, uniform int8 int8_2,
                                                            uniform int8 int8_3) {
    uniform nestedEmpty struc = getWithNestedEmptyStructFromCpp(int16_1, struct1, int8_1, int8_2, int8_3);
    assert(struc.a == int16_1 && struc.b.x == struct1->x && struc.c == int8_1 && struc.d == int8_2 &&
           struc.e == int8_3);
    return struc;
}
