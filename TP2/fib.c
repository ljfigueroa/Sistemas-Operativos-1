#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

void *fib(void *p)
{
  unsigned int n = *(int*)p;

  if (n < 2)
    return NULL;
  else {
    int x, y;
    pthread_t t1, t2;
    
    x = n-1;
    y = n-2;
    pthread_create(&t1, 0,fib, &x); 
    pthread_create(&t2, 0,fib, &y); 

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    *(int*)p = x+y;
  }
  return NULL;
}


int main(int argc, char *argv[])
{
  int n;

  if (argc != 2) {
    fprintf(stderr, "Usage: fib [<cilk options>] <n>\n");
    return 0;
  }
  n = atoi(argv[1]);
  fib(&n);

  printf("Result: %d\n", n);
  return 0;
}
