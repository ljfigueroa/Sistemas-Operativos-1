#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#define MAX_THREADS 3
#define MIN_FIB 30

int threads_activos;
pthread_mutex_t l = PTHREAD_MUTEX_INITIALIZER;

int fib_secuencial(int n) 
{
  if(n < 2) 
    return n;
 
  return (fib_secuencial(n-1) + fib_secuencial(n-2));
}


void *fib(void *p)
{
  unsigned int n = *(int*)p;

  if (n < 2)
    return NULL;
  else if(threads_activos >= MAX_THREADS || n <= MIN_FIB) {
    *(int*)p = fib_secuencial(n);
    return NULL;
  } else {
    pthread_mutex_lock(&l);
    threads_activos+=2;
    pthread_mutex_unlock(&l);

    int x, y;
    pthread_t t1, t2;
    
    x = n-1;
    y = n-2;
    pthread_create(&t1, 0,fib, &x); 
    pthread_create(&t2, 0,fib, &y); 

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    pthread_mutex_lock(&l);
    threads_activos-=2;
    pthread_mutex_unlock(&l);

    *(int*)p = x+y;
  }
  return NULL;
}


int main(int argc, char *argv[])
{
  int n;
  threads_activos = 0;

  if (argc != 2) {
    fprintf(stderr, "Usage: fib [<cilk options>] <n>\n");
    return 0;
  }
  n = atoi(argv[1]);
  fib(&n);

  printf("Result: %d\n", n);
  return 0;
}

