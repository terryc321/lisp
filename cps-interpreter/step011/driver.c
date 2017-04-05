
#include <stdlib.h>
#include <stdio.h>

extern int scheme_entry(char *heap);

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

#define PAIR_MASK  7
#define PAIR_TAG   1





/* heap size should be a multiple of 8 bytes  */
#define HEAP_SIZE  (8*100000)

void write(int val){

  /*
  small fixnums 

      2   1  bits 
      0   0  values

  mask = 2 + 1 = 3
  tag  = 0
  */
  if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
    printf("%d", val >> FIXNUM_SHIFT);
  }
  /*
  characters 

     128  64  32 16  8  4  2   1  bits
       0   0   0  0  1  1  1   1  values

  mask = 128 + 64 + 32 + 16 + 8 + 4 + 2 + 1 = 255
  tag  =   8 + 4 + 2 + 1 =  15
  */
  else if ((val & CHAR_MASK ) == CHAR_TAG ) {
    printf("#%c%c", '\\' ,val >> CHAR_SHIFT);
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
      printf("#t");
    }
    else{
      printf("#f");
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
      printf("()");
  }
  
  /*
  Pairs

           4  2  1  bits 
   - - -   0  0  1  values

  mask = 4 + 2 + 1 = 7
  tag  = 1
  */
  else if ((val & PAIR_MASK ) == PAIR_TAG ) {
    /* un tag */
    int *the_car = (int *)(val - 1);
    int *the_cdr = (int *)(val + 3);
    printf("(");
    write(*the_car);
    printf(" . ");
    write(*the_cdr);
    printf(")");    
    
  }
  
  else {
    /*
    dont know , dont ask me.
     */
    printf("??VAL = %p\n", (void *) val);
  }

}




int main(int argc, char **argv){
  char *ptr = (char *)malloc(sizeof(HEAP_SIZE));
  if (!ptr){
    
    return 1;
  }
  
  int val = scheme_entry(ptr);

  write((int)val);
  
  fflush(stdout);
  
  if (ptr){
    free((void *)ptr);
    ptr = 0;
  }
  
  return 0;
}















