template<typename T, typename C>
uniform T goo(uniform const T arg0, uniform C arg1) {
  //T val = 0;
  return arg0 * 5;
}

export uniform float foo(uniform int arg) {
  uniform int a = goo<int, int>(arg, arg);
  a = goo<int, float>(arg, arg);
  return a;
}

/*float foo(int arg) {
  //int a = goo<int, int>(arg);
  return arg;
}*/
