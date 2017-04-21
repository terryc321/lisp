
#include <stdio.h>

extern int scheme_entry();

#define FIXNUM_MASK   3
#define FIXNUM_TAG    0
#define FIXNUM_SHIFT  2

#define CHAR_MASK 255
#define CHAR_TAG  15
#define CHAR_SHIFT 8


#define BOOL_MASK 127
#define BOOL_TAG  31
#define BOOL_SHIFT 8
#define BOOL_BIT  128 


int main(int argc, char **argv){
  int val = scheme_entry();
  
  /*
  small fixnums 

      2   1  bits 
      0   0  values

  mask = 2 + 1 = 3
  tag  = 0
  */
  if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
    printf("%d\n", val >> FIXNUM_SHIFT);
  }
  /*
  characters 

     128  64  32 16  8  4  2   1  bits
       0   0   0  0  1  1  1   1  values

  mask = 128 + 64 + 32 + 16 + 8 + 4 + 2 + 1 = 255
  tag  =   8 + 4 + 2 + 1 =  15
  */
  else if ((val & CHAR_MASK ) == CHAR_TAG ) {
    printf("#%c%c\n", '\\' ,val >> CHAR_SHIFT);
  }

  
  /*
  booleans 
     128  64  32  16   8   4   2   1  bits
      X    0   0   1   1   1   1   1  values

  mask = 64 + 32 + 16 + 8 + 4 + 2 + 1  = 127
  tag  = 16 + 8 + 4 + 2 + 1  = 31
  if #t then X = 1
  if #f then X = 0

   */
  else if ((val & BOOL_MASK ) == BOOL_TAG ) {
    if ( val & BOOL_BIT ){
      printf("#t\n");
    }
    else{
      printf("#f\n");
    }
  }

  /*

  the empty list

     128  64  32  16   8   4   2   1  bits
      -   -    1   0   1   1   1   1  values

  mask = 
  tag  = 1 + 2 + 4 + 8 + 32 = 47

   */
  
  else if (val == 47 ){
      printf("()\n");
  }
  
  else {
    /*
    dont know , dont ask me.
     */
    printf("??\n");
  }
  return 0;
}















