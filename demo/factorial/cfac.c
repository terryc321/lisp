#include <stdio.h>
#include <stdlib.h>

int fac(int n){
  if (n == 1){
    return 1;
  }
  return n * fac(n - 1);
}

