/*
 * TO-CHECK

FAIL 1
Link : https://godbolt.org/z/EzTEfq
unmasked uniform int16 foo(float maxVal) {
    uniform int16 maxValRet = @llvm.convert.to.fp16.f32(maxVal);
    return maxValRet;
}

export void goo(uniform float inVal[], uniform int16 outVal[]) {
    float valIn = inVal[programIndex];
    int16 valRet = foo(valIn);
    outVal[programIndex] = valRet;
}

*/

export void f_f(uniform float RET[], uniform float aFOO[]) {

    RET[programIndex] = 0;

    // Case 1:
    // To-DO : Figure out check
    uniform float delta = 0.5;
    uniform float maxVal = 31743;
    uniform int16 maxValRet = @llvm.convert.to.fp16.f32(maxVal);
    print("\n maxVal = %, maxValRet = %, \n", maxVal, maxValRet);
    uniform float maxValMinus = maxVal - delta;;
    uniform int16 maxValMinusRet = @llvm.convert.to.fp16.f32(maxValMinus);
    print("\n maxValMinus = %, maxValMinusRet = %, \n", maxValMinus, maxValMinusRet);
    uniform float maxValPlus = maxVal - delta;;
    uniform int16 maxValPlusRet = @llvm.convert.to.fp16.f32(maxValPlus);
    print("\n maxValPlus = %, maxValPlusRet = %, \n", maxValPlus, maxValPlusRet);

    // Case 2:
    float matrix = aFOO[programIndex];
    uniform float t_matriz[programCount];
    uniform int row = 0;
    uniform int column = 0;
    float transpose = aFOO[programIndex];
#if TARGET_WIDTH == 4
    row = 2;
    column = 2;
    transpose = @llvm.matrix.transpose.v8f32.i32.i32(matrix, 2, 2);
#elif TARGET_WIDTH == 8
    row = 2;
    column = 4;
    transpose = @llvm.matrix.transpose.v8f32.i32.i32(matrix, 2, 4);
#elif TARGET_WIDTH == 16
    row = 4;
    column = 4;
    transpose = @llvm.matrix.transpose.v8f32.i32.i32(matrix, 4, 4);
#endif
    t_matriz[programIndex] = transpose;

    print("\n matrix = % \n transpose = % \n", matrix, transpose);
    for (uniform int r = 0; r < row; r++) {
        for (uniform int c = 0; c < column; c++) {
            if (t_matriz[r * row + c] != aFOO[c * row + r]) {
                RET[1] = 1;
            }
        }
    }
}

export void result(uniform float RET[]) {
    RET[programIndex] = 0;
}




