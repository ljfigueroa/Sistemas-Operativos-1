#include <stdio.h>
#include <pthread.h>

#define N_VISITANTES 1000000
int visitantes = 0;
pthread_mutex_t l = PTHREAD_MUTEX_INITIALIZER;

void molinete()
{
  int i;
  for (i=0;i<N_VISITANTES;i++) {
    pthread_mutex_lock(&l);
    visitantes++;
    pthread_mutex_unlock(&l);
  }
}

int main ()
{
  pthread_t molinete1, molinete2;
  pthread_create(&molinete1, 0, molinete, NULL);
  pthread_create(&molinete2, 0, molinete, NULL);
  pthread_join(molinete1, NULL);   
  pthread_join(molinete2, NULL);   

  printf("Hoy hubo %d visitantes!\n", visitantes);
  return 0;
}
