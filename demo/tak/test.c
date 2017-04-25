

#include <stdio.h>

extern int tak1(int x, int y , int z);
extern int tak2(int x, int y , int z);
extern int tak3(int x, int y , int z);
extern int tak(int x, int y , int z);



int main(int argc, char **argv){
  int x = 12;
  int y = 8;
  int z = 6;
  printf("x = %d \n" , tak1(x,y,z));
  printf("y = %d \n" , tak2(x,y,z));
  printf("z = %d \n" , tak3(x,y,z));

  printf("TAK %d %d %d = %d \n" , x ,y , z , tak(x,y,z));
  
  return 0;
}








