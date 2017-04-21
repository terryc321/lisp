
#include <stdio.h>

extern int scheme_entry();


int main(int argc, char **argv){
  int val = scheme_entry();
  if ((val % 4) == 0) {
    printf("%d\n", val >> 2);
  }
  else {
    printf("??\n");
  }
  return 0;
}






