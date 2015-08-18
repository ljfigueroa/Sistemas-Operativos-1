#include <stdio.h>
#include <pthread.h>
#include <assert.h>

#define N		20
#define ARRLEN	1024

int arr[ARRLEN];
pthread_mutex_t l = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t cond2 = PTHREAD_COND_INITIALIZER;
int lectores = 0;
int escritores = 0;

void *escritor(void *arg)
{
  int i;
  int num = *((int *)arg);
  for (;;) {
    pthread_mutex_lock(&l);
    escritores++;
    printf("Escritor %i adentro\n", num);
    sleep(random()%1);
    while(lectores > 0)
        pthread_cond_wait(&cond, &l);
    for (i=ARRLEN-1; i>=0; i--) {
      arr[i] = num;
    }
    printf("Escritor %i afuera\n", num);
    escritores--;
    pthread_cond_signal(&cond2);
    pthread_mutex_unlock(&l);
  }
  return NULL;
}

void *lector(void *arg)
{
  int v, i, err;
  int num = *((int *)arg);

  for (;;) {
    pthread_mutex_lock(&l);

    lectores++;
    pthread_mutex_unlock(&l);

    sleep(random()%1);
    err = 0;
    v = arr[0];
    for (i=1; i<ARRLEN; i++) {
      if (arr[i]!=v) {
        err=1;
        break;
      }
    }
    printf("lector fuera \n");

    if (err) printf("Lector %d, error de lectura\n", num);
    else printf("Lector %d, dato %d\n", num, v);

    lectores--;
    if(lectores == 0)
        pthread_cond_signal(&cond);
    while(escritores > 0)
        pthread_cond_wait(&cond2, &l);

    pthread_mutex_unlock(&l);
  }
  return NULL;
}

int main()
{
  int i;
  pthread_t lectores[N], escritores[N];
  int arg[N];

  for (i=0; i<ARRLEN; i++) {
    arr[i] = -1;
  }
  for (i=0; i<N; i++) {
    arg[i] = i;
    pthread_create(&lectores[i], NULL, lector, (void *)&arg[i]);
    pthread_create(&escritores[i], NULL, escritor, (void *)&arg[i]);
  }
  printf(" FUERA DEL FOR \n");
  pthread_join(lectores[0], NULL); /* Espera para siempre */
  return 0;
}
