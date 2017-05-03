#include <stdio.h>
#include <stdlib.h>



extern int fac(int n);

int main(int argc, char **argv){
  int i = 1;
  int j = 1;
  for (i = 1 ; i < 10 ; i ++){
    for (j = 1; j < 1000000; j ++){
      fac(i);
    }
    printf("i = %d , fac(i) = %d \n" , i , fac(i));
  }
  return 0;
}




