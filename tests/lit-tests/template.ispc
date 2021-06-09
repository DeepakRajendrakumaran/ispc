template<typename T, typename C>
uniform int goo(uniform const T arg0, uniform int arg1) {
  C val = 0;
  return val * 5;
}

export uniform float foo(uniform int arg, uniform int arg1) {
  uniform int a = goo<int, int>(arg, arg);
  uniform int b = goo<float, int>(arg1, arg1);
  return a;
}

/*float foo(int arg) {
  //int a = goo<int, int>(arg);
  return arg;
}*/
