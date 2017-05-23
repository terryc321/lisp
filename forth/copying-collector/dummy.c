
#include <stdio.h>
#include <stdlib.h>

extern void heap_fill(int *heap);
extern void heap_clear(int *heap);

extern void bitmap_clear(int *bitmap, int size);
extern void bitmap_fill(int *bitmap, int size);
extern void bitclr(int *bitmap, int n);
extern void bitset(int *bitmap, int n);
extern int bittst(int *bitmap, int n);


extern int *heap1_addr();
extern int *heap2_addr();
extern int *bitmap1_addr();
extern int *bitmap2_addr();
extern int heap_size();

extern int *alloc(int n);

// only for side effect
void cons();

void show_bitmap(int *bitmap, int size);
void show_bitmap_byte(int byte);

void show_bitmap_byte(int byte){
  //printf("%d ", byte);

  int i = 0;
  for (i = 0 ; i < 32 ; i ++){
    printf("%d ", (byte & (1 << i)) ? 1 : 0  );
  }
  
}

void show_bitmap(int *bitmap, int size){
  int i = 0;
  int n = 0;
  for ( i=0 ;i < size ; i ++){
    show_bitmap_byte(bitmap[i]);
    printf("\n");
  }  
}




// cons may cause a cascade garbage collection
// only for side effect
// every routine has this 
void cons(){
  
}






int main(int argc, char **argv){
  printf("hello world\n");

  
  cons(1,2);
  
  int *b1 = bitmap1_addr();   
  int *b2 = bitmap2_addr();
  int *b3 = (int *)malloc(sizeof(int) * 5);
  if (!b3){
    printf("\nmalloc failed on B3 \n");
  }
  int b4[10];
   
  int i = 0 ;
  int n = 0 ;
  
  printf("bitmap 1 address = %p \n", b1);  
  printf("bitmap 2 address = %p \n", b2);  
  printf("bitmap 3 address = %p \n", b3);
  printf("bitmap 4 address = %p \n", b4);

  printf("\n\nClearing bitmap 3 \n\n");  
  bitmap_clear(b3,10);
  printf("\n\n");

  printf("\n\nClearing bitmap 4 \n\n");  
  bitmap_clear(b4,10);
  printf("\n\n");

  // ***********************************************
  
  printf("\n\nClearing bitmap1 \n\n");  
  bitmap_clear(b1,1);
  printf("\n\n");
  for ( n = 0 ;  n < 1 * 32 ; n ++){
    printf("\nsetting %d th bit of bitmap 1 \n", n);
    bitset(b1, n);
    show_bitmap(b1, 1);  

    int k = 0;
    for ( k = 0 ; k < 1 * 32 ; k ++){
      if (bittst(b1,k)){
	printf("%d : it is set\n",k);
      }
      else {
	printf("%d : it is NOT set\n",k);      
      }
    }
    
    printf("\nclearing %d th bit of bitmap 1 \n", n);
    bitclr(b1, n);    
    show_bitmap(b1, 1);      
  }

  return 0;
  
  printf("\n\nFilling bitmap 1 \n\n");  
  bitmap_fill(b1 , 1);
  show_bitmap(b1, 1);
 
  printf("\nCheck bitmap 2 \n");  
  show_bitmap(b2, 10);  

  printf("\nClearing bitmap 3 \n");  
  bitmap_clear(b3,10);

  


  // ***********************************************

  b2 = bitmap2_addr();   
  printf("bitmap 2 address = %p \n", b1);  
  
  printf("\n\nClearing bitmap2 \n\n");  
  bitmap_clear(b2,10);
  
  printf("\n\nFilling bitmap2 \n\n");  
  bitmap_fill(b2,10);
  show_bitmap(b2, 10);  
  
  printf("\n\n");
  for ( n = 0 ;  n < 10 * 32 ; n ++){
    printf("\nsetting %d th bit of bitmap 2 \n", n);
    bitset(b2, n);
    show_bitmap(b2, 10);  
    
    printf("\nclearing %d th bit of bitmap 2 \n", n);
    bitclr(b2, n);    
    show_bitmap(b2, 10);  
    printf("\n\n");
  }

  
  
  /*
  
  b1[0] = (1 << 0) | (1 << 2) | (1 << 4) | (1 << 5) | ( 1 << 6) | (1 << 29) | (1 << 31) ;
  printf("\nbitmap 1 \n");
  show_bitmap(b1, 10);

  printf("\n\nClearing bitmap1 \n\n");  
  bitmap_clear(b1,10);
  
  printf("\n\n");
  int n = 0;
  for ( n = 0 ;  n < 10 * 32 ; n ++){
    bitset(b1, n);
    printf("\nset %d th bit of bitmap \n", n);
    show_bitmap(b1, 10);  
    printf("\n\n");
  }
  
  printf("\n\n");
  n = 0;
  for ( n = 0 ;  n < 10 * 32 ; n ++){
    bitclr(b1, n);
    printf("\nclearing %d th bit of bitmap \n", n);
    show_bitmap(b1, 10);  
    printf("\n\n");
  }
  

  printf("\n\nClearing bitmap1 \n\n");  
  bitmap_clear(b1,10);
  
  

  */
  
  return 0;
  
}


