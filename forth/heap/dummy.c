
#include <stdio.h>
#include <stdlib.h>

extern void heap1_fill();
extern int *heap1_addr();
extern int *heap2_addr();
extern int heap_size();

static int *h1;
static int *h2;

int main(int argc, char **argv){
  printf("hello world\n");
  printf("heap size = %d\n", heap_size());

  h1 = heap1_addr();
  h2 = heap2_addr();
    
  printf("heap 1 address = %p \n", h1);
  printf("heap 2 address = %p \n", h2);

  
  int i = 0 ;

  while(1){
  
  printf("\nheap 1 \n");
  for ( i=0 ;i < heap_size() ; i ++){
    printf("%d ", h1[i]);
  }

  printf("\nheap 2 \n");
  for ( i=0 ;i < heap_size() ; i ++){
    printf("%d ", h2[i]);
  }

  printf("\n\nFilling heap 1 \n\n");
  
  heap1_fill();
  
  printf("\nheap 1 \n");
  for ( i=0 ;i < heap_size() ; i ++){
    printf("%d ", h1[i]);
  }

  printf("\nheap 2 \n");
  for ( i=0 ;i < heap_size() ; i ++){
    printf("%d ", h2[i]);
  }

  
  printf("\n\n");
  }

  return 0;
  
}


