
#include <stdio.h>
#include <stdlib.h>


/*

want to align the HEAP pointer to be on 8 byte boundary

ie lower 3 bits are zero


 */

int main(int argc, char **argv){
  int i;
  for (i = 0 ; i < 100 ; i++){
    int addr = i;
    int next = i;
    //next = next | 8;
    //next = next + 8;
    next = next + atoi(argv[1]);
    //next = next & (~(0b111));
    next = next & (-8);
        
    // code ensure next is aligned on 8 byte boundary
    if ((next % 8) == 0){
      printf("address %d : aligned %d \n", addr , next);
    }
    else{
      printf("address %d : mis aligned %d \n" , addr , next);
      break;
    }
  }  
  return 0;
}





