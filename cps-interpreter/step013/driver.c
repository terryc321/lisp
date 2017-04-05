
#include <stdlib.h>
#include <stdio.h>

extern unsigned int scheme_entry(char *heap);
extern unsigned int scheme_car(unsigned int ptr);
extern unsigned int scheme_cdr(unsigned int ptr);

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
#define HEAP_SIZE  1000


void pretty_print(unsigned int val){

  //int val = (int)(valptr);
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
    /* un tag - take off 1 byte , not 4 bytes if int * ptr */
    unsigned int the_car = scheme_car(val);
    unsigned int the_cdr = scheme_cdr(val);

    /*
    printf("\nthe CAR address is %p " , the_car);
    printf("\nthe CDR address is %p " , the_cdr);
    printf("\n");
    */
    
    printf("(");
    pretty_print(the_car);
    printf(" . ");
    pretty_print(the_cdr);
    printf(")");    

    
  }
  
  else {
    /*
    dont know , dont ask me.
     */
    printf("??VAL.dec = %d\n",  val);
    printf("??VAL.ptr = %p\n",  (void *) val);
    printf("??VAL + pair_mask = %p\n",  (void *) (val & PAIR_MASK));
    
  }

}




int main(int argc, char **argv){
  
  char *ptr = (char *)malloc(sizeof(int) * HEAP_SIZE);
  if (!ptr){   
    return 1;
  }

  
  int i = 0 ;

  
  for (i = 0 ; i < (HEAP_SIZE) ; i ++){
    ptr[i] = 0;
  }
  

  //printf("\nHEAP address is [ %p ] \n", (void *)ptr);

  
  //  for (i = 0 ; i < 100 ; i ++){
  unsigned int val = scheme_entry(ptr);

  /*
  //char *val = ptr;
   
  printf("\nVAL = %d " , val);

  int *iptr = (int *) ptr;
    
  iptr[0] = 4; // 0
  iptr[1] = 8; // 4
  iptr[2] = 12; // 8 
  iptr[3] = 16; // 12
  
  char *ptr2 = 0;
  ptr2 = ptr + 1;
  iptr[4] = ((int) ptr2); // 16
  ptr2 = ptr + 9;
  iptr[5] = ((int) ptr2); // 20

  ptr2 = ptr + 16;
  val = ptr2;

  
  for (i = 0 ; i < 12 ; i ++){
    printf("\n%p: 0x%x" , iptr + i,  iptr[i]);
  }


  */
  
  
  // printf("\n--------------------------\n");
  pretty_print(val);
  //printf("\n--------------------------\n");
  
  
  //}
  
  fflush(stdout);
  
  
  if (ptr){
    free((void *)ptr);
    ptr = 0;
  }
  
  return 0;
}

















