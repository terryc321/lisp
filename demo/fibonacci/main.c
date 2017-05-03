#include <stdio.h>
#include <stdlib.h>



extern int fib(int n);

int main(int argc, char **argv){
  int i = 1;
  int j = 1;
  for (i = 1 ; i < 30 ; i ++){
    /* for (j = 1; j < 1000000; j ++){ */
    /*   fib(i); */
    /* } */
    printf("i = %d , fib(i) = %d \n" , i , fib(i));
  }
  return 0;
}






