


void suzanne(int *ptr){
  ptr[0] = 1;
  ptr[1] = 2;
  ptr[2] = 3;
  ptr[3] = 4;
}


int main(int argc,char **argv){
  int buffer[10];
  int *ptr = buffer;
  suzanne(ptr);
  return 0;
}



