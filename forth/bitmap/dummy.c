
#include <stdio.h>
#include <stdlib.h>

extern void heap_fill(int *heap);
extern void heap_clear(int *heap);
extern int *heap1_addr();
extern int *heap2_addr();
extern int *bitmap1_addr();
extern int *bitmap2_addr();
extern int heap_size();

static int *h1;
static int *h2;
static int *b1;
static int *b2;

void show_arr(int *arr, int size);
void show_arr(int *arr, int size){
  int i = 0;
  int n = 0;
  for ( i=0 ;i < size ; i ++){
    printf("%d ", arr[i]);
    n ++;
    if (n > 7){
      n = 0;
      printf("\n");
    }
  }  
}




int main(int argc, char **argv){
  printf("hello world\n");
  printf("heap size = %d\n", heap_size());

  h1 = heap1_addr();
  h2 = heap2_addr();
  b1 = bitmap1_addr();
  b2 = bitmap2_addr();
    
  printf("heap 1 address = %p \n", h1);
  printf("heap 2 address = %p \n", h2);
  printf("bitmap 1 address = %p \n", b1);
  printf("bitmap 2 address = %p \n", b2);

  
  int i = 0 ;

  heap_fill(h1);
  heap_fill(h2);
  
  
  printf("\nheap 1 \n");
  show_arr(h1, heap_size());
  
  printf("\nheap 2 \n");
  show_arr(h2, heap_size());

  printf("\n\nClearing heap 1 \n\n");  
  heap_clear(h1);
  
  
  printf("\nheap 1 \n");
  show_arr(h1, heap_size());

  printf("\n\nClearing heap 2 \n\n");  
  heap_clear(h2);
  
  printf("\nheap 2 \n");
  show_arr(h2, heap_size());
  
  
  printf("\n\n");

  return 0;
  
}


