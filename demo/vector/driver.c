
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

void scheme_pretty_print(unsigned int val);
void scheme_pretty_print_nl(unsigned int val);

char *scheme_make_vector(int num);
int scheme_vector_set(int val, int offset, int vec);
int scheme_vector_ref(int offset, int vec);

char *scheme_cons(int b, int a);
char *scheme_closure(int n, ...);

extern unsigned int scheme_entry();
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

#define PAIR_MASK    7
#define PAIR_TAG     1

#define VECTOR_MASK  7
#define VECTOR_TAG   2

#define STRING_MASK  7
#define STRING_TAG   3


#define SYMBOL_MASK  7
#define SYMBOL_TAG   5


#define CLOSURE_MASK 7
#define CLOSURE_TAG  6

#define FALSE_VALUE   31
#define TRUE_VALUE    159

/* heap size should be a multiple of 8 bytes  */
#define HEAP_SIZE  (32 * 1000000)



/*
 garbage collector for scheme lisp
 cons called expects memory region it write to be in the ESI register
 c returns values in EAX register
 after allocate call


 allocate N cells of memory , ideally a multiple of 8 bytes or 2 * N cells 
 e.g 2 , 4 , 6 , 8 etc...
 allocate ( 2 ) 

 mov dword esi , eax

two heaps from heap , to heap 

a number of bitmaps also to indicate if pointer is to an object in the heap
bitmap if object starts at the address.
bitmap if object has been forwarded yet.


char * ptr so we can access every individual byte
on 32 bit machine , int takes 4 bytes 

*/

// allocptr is used by the allocator
// it is a byte pointer , why ? , maybe a
// 
static char *allocptr;

static char *heap_to;
static char *heap_from;


// might only need one forwarded bitmap
// have two for now
static int *bitmap_forwarded_to;
static int *bitmap_forwarded_from;
static int bitmap_size;

// tell us after we have untagged a heap pointer ,
// if that pointer is to an object in the heap ,
// or its something else
static int *bitmap_isobject_to;
static int *bitmap_isobject_from;

// belief that no pointer should point into a particular object
// e.g
// closure-ref
// vector-ref
// string-ref





char *scheme_cons(int b, int a){
  // 
  int *res = (int *)allocptr;
  res[0] = a;
  res[1] = b;

  allocptr = allocptr + 8;
  
  if ( (((int)allocptr) % 8) != 0 ){
    allocptr = allocptr + 4;
  }  
  
  int ptr = (int)res;
  ptr = ptr | PAIR_TAG ;
  return (char *)ptr;
}




char *scheme_closure(int num, ...){
  // fancy pants variable argument procedure in C , o-o .
  // that builds the closure data structure
  // really just like code to build a vector
  int *res = (int *)allocptr;
  
  va_list arguments;
  va_start(arguments , num);
  int i = 0;
  for (i = 0 ; i < num ; i ++){
    res[i] = va_arg(arguments , int);
    allocptr = allocptr + 4;
  }
  va_end(arguments);
  
  if ( (((int)allocptr) % 8) != 0 ){
    allocptr = allocptr + 4;
  }  
  int ptr =(int)res;
  ptr = ptr | CLOSURE_TAG ; 
  return (char *)ptr;
}






char *scheme_make_vector(int num){
  // not really sure these warnings are valid since
  // not using any registers directly anymore.
  // e.g old version ESI register was the HEAP allocator bump pointer.
  
  // ??important - we do NOT make a C library system call in this routine  
  // ??no registers are preserved 
  printf("making a vector of size [%d]\n",num);

  // vector format = [ SIZE-of-Vector-untagged ELEM-1 ELEM-2 ELEM-3 ... ]
  int *res = (int *)allocptr;
  res[0] = num;
  allocptr = allocptr + 4;
    
  int i = 0;
  for (i = 0 ; i < num ; i ++){
    // the false value
    res[1 + i] = FALSE_VALUE;
    allocptr = allocptr + 4;
  }

  
  if ( (((int)allocptr) % 8) != 0 ){
    allocptr = allocptr + 4;
  }

  int ptr = (int)res;
  ptr = ptr | VECTOR_TAG ;  
  return (char *)ptr;
}







int scheme_vector_set(int val, int offset, int vec){

  // untag vector
  int vecptr = (int) vec;
  vecptr = vecptr & ( -8 );
  
  // untag offset (assuming its a fixnum)
  int index = offset >> 2 ;

  //printf("vector_set : index = [%d] : value \n",index);
  
  int *vptr = (int *)vecptr;
  vptr[index + 1] = val;
  
  // store value in the vector  
  return FALSE_VALUE;
}



int scheme_vector_ref(int offset, int vec){

  // untag vector
  int vecptr = (int) vec;
  vecptr = vecptr & ( -8 );
  
  // untag offset (assuming its a fixnum)
  int index = offset >> 2 ;

  //printf("vector_ref : index = [%d] : value \n",index);
  
  int *vptr = (int *)vecptr;
  return vptr[index + 1];  
}





void debug_stack(unsigned int *ptr){
  printf("\nSTACK : ");
  int i = 0 ;
  for (i = 0 ; i > -5 ; i = i - 1){
    printf("%p " , (void *)ptr[i]);
  }
    
}


void scheme_pretty_print_nl(unsigned int val){
  scheme_pretty_print(val);
  printf("\n");
}



void scheme_pretty_print(unsigned int val){

  //int val = (int)(valptr);
  /*
  small fixnums 

      2   1  bits 
      0   0  values

  mask = 2 + 1 = 3
  tag  = 0
  */
  if ((val & FIXNUM_MASK) == FIXNUM_TAG) {
    int signed_val = (int) val;
    printf("%d", signed_val >> FIXNUM_SHIFT);
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

    /*
    printf("\nthe CAR address is %p " , the_car);
    printf("\nthe CDR address is %p " , the_cdr);
    printf("\n");
    */
    
    printf("(");

    while(1){
          unsigned int the_car = scheme_car(val);
	  scheme_pretty_print(the_car);

	  val = scheme_cdr(val);

	  if (47 == val){
	    // empty list = end of the list
	    break;
	  }
	  else if ((val & PAIR_MASK ) == PAIR_TAG) {
	    // still a CONS pair , thats okay , keep going
	    printf(" ");	    
	  }
	  else {
	    printf(" . ");
	    scheme_pretty_print(val);
	    break;
	  }

    }
    
    printf(")");    
    
  }
  /* 
  Vectors

           4 2 1  bits
           0 1 0  values
   vector mask = 4 + 2 + 1 = 7
   vector tag  = 2
   */
  else if ((val & VECTOR_MASK ) == VECTOR_TAG ) {

    int *ptr = (int *)(val - VECTOR_TAG);
    int size = ptr[0];
    //size = size >> 2 ;
    printf("#[");
    //printf("%d " , size);    
    int n = 0 ;
    for (n = 1 ; n <= size ; n ++){
      //printf("_");
      scheme_pretty_print(ptr[n]);
      if (n == size){
	//
      }
      else {
	printf(" ");
      }
    }    
    printf("]");    
    
  }
  else if ((val & CLOSURE_MASK ) == CLOSURE_TAG ) {
    int *ptr = (int *)(val - CLOSURE_TAG);
    printf("#<closure %p>" , (void *)ptr); //, (void *)ptr);    
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

  // ------------------ allocate heaps --- FROM HEAP ----------
  heap_from = (char *)malloc(HEAP_SIZE);  
  if (!heap_from){   
    return 1;
  }

  if (((int)heap_from) % 8 == 0){
    // heap aligned ok.
  }
  else {
    printf("heap_from is misaligned .");
    return 2;
  }

  // ------------------ allocate heaps --- TO HEAP -----------
  heap_to = (char *)malloc(HEAP_SIZE);  
  if (!heap_to){   
    return 1;
  }

  if (((int)heap_to) % 8 == 0){
    // heap aligned ok.
  }
  else {
    printf("heap_to is misaligned .");
    return 2;
  }

  // setup allocptr
  allocptr = heap_from;

  // setup bitmaps
  bitmap_size = (HEAP_SIZE / 32) + 4;
  bitmap_forwarded_to = (int *)malloc(sizeof(int) * bitmap_size);
  bitmap_forwarded_from = (int *)malloc(sizeof(int) *bitmap_size);
  bitmap_isobject_to = (int *)malloc(sizeof(int) *bitmap_size);
  bitmap_isobject_from = (int *)malloc(sizeof(int) *bitmap_size);

  printf("--- before cleaning ---\n");
  printf("heap_from = %p\n",heap_from);
  printf("heap_to = %p\n",heap_to);
  printf("bitmap_size = %d\n",bitmap_size);
  
  printf("bitmap_forwarded_from = %p\n",bitmap_forwarded_from);
  printf("bitmap_forwarded_to = %p\n",bitmap_forwarded_to);
  printf("bitmap_isobject_from = %p\n",bitmap_isobject_from);
  printf("bitmap_isobject_to = %p\n",bitmap_isobject_to);
  printf("--- after cleaning ---\n");
  
  printf("bitmap_size = %d \n" , bitmap_size);
  if ( !allocptr
       || !heap_from
       || !heap_to
       || !bitmap_forwarded_to
       || !bitmap_forwarded_from
       || !bitmap_isobject_to
       || !bitmap_isobject_from ){
    printf("Insufficient Memory - exiting before things get much worse.\n");
    return 3;
  }

  // ------- clean all bitmaps -----------
  int i = 0 ;  
  
  for (i = 0 ; i < (bitmap_size - 1) ; i ++){
    bitmap_forwarded_to[i] = 0;
  }    
  for (i = 0 ; i < bitmap_size ; i ++){
    bitmap_forwarded_from[i] = 0;
  }
  for (i = 0 ; i < bitmap_size ; i ++){
    bitmap_isobject_to[i] = 0;
  }
  for (i = 0 ; i < bitmap_size ; i ++){
    bitmap_isobject_from[i] = 0;
  }

    
  // we may choose to clear the heaps at startup 
  for (i = 0 ; i < (HEAP_SIZE ) ; i ++){
    heap_from[i] = 0;
  }
  for (i = 0 ; i < (HEAP_SIZE / 4) ; i ++){
    heap_to[i] = 0;
  }
  
  
  
  printf("heap_from = %p\n",heap_from);
  printf("heap_to = %p\n",heap_to);
  printf("bitmap_forwarded_from = %p\n",bitmap_forwarded_from);
  printf("bitmap_forwarded_to = %p\n",bitmap_forwarded_to);
  printf("bitmap_isobject_from = %p\n",bitmap_isobject_from);
  printf("bitmap_isobject_to = %p\n",bitmap_isobject_to);

  
  
  /*

  FILE *fp = fopen("test.log","w");
  if (!fp){
    return 2;
  }
  fprintf(fp,"\nDriver.c THe HEAP address is [ %p ] \n", (void *)ptr);
  fflush(fp);

  fclose(fp);
  */



  // calling scheme_entry 
  
  // run the program a number of times
  unsigned int val = 0;
  //for (i = 0 ; i < 10000 ; i ++){
  val = scheme_entry();
    //}
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
  //scheme_pretty_print(val);
  //printf("\n--------------------------\n");

  
  
  //}
  
  fflush(stdout);
  
  
  if (heap_from){
    free((void *)heap_from);
    heap_from = 0;
  }
  
  if (heap_to){
    free((void *)heap_to);
    heap_to = 0;
  }
  
  return 0;
}

















