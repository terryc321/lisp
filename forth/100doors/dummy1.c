
#include <stdio.h>
#include <stdlib.h>

extern void heap1_fill();
extern int *heap1_addr();
extern int *heap2_addr();
extern int heap_size();
extern void toggleEvery(int n);

static int *h1;
static int *h2;

int main(int argc, char **argv){
  printf("hello world\n");
  printf("heap size = %d\n", heap_size());

  h1 = heap1_addr();
    
  printf("heap 1 address = %p \n", h1);

  
  int i = 0 ;

  
  printf("\nheap 1 \n");
  for ( i=0 ;i < heap_size() ; i ++){
    printf("%d ", h1[i]);
  }

  printf("\n\nToggling doors \n\n");
  
  //toggleEvery(2);
  toggleAll(); 
  
  printf("\nheap 1 \n");
  for ( i=0 ;i < heap_size() ; i ++){
    printf("%d : %d \n", i + 1, h1[i]);
  }

  
  printf("\n\n");

  return 0;
  
}



