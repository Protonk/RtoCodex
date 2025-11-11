// Simple C routine used by add1()
#include <R.h>
void C_add1(int *x, int *n) {
  for (int i = 0; i < *n; i++) x[i] = x[i] + 1;
}
