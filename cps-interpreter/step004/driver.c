
#include <stdio.h>

extern int scheme_entry();

#define FIXNUM_MASK   3
#define FIXNUM_TAG    0
#define FIXNUM_SHIFT  2

#define CHAR_MASK 255
#define CHAR_TAG  15
#define CHAR_SHIFT 8


int main(int argc, char **argv){
  int val = scheme_entry();
  
  /*  2   1  bits 
      0   0  values
  small fixnums 
  mask = 2 + 1 = 3
  tag  = 0
  */
  if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
    printf("%d\n", val >> FIXNUM_SHIFT);
  }
  /* 128  64  32 16  8  4  2   1  bits
       0   0   0  0  1  1  1   1  values
  characters 
  mask = 128 + 64 + 32 + 16 + 8 + 4 + 2 + 1 = 255
  tag  =   8 + 4 + 2 + 1 =  15
  */
  else if ((val & CHAR_MASK ) == CHAR_TAG ) {
    printf("#%c%c\n", '\\' ,val >> CHAR_SHIFT);
  }
  else {
    printf("??\n");
  }
  return 0;
}












