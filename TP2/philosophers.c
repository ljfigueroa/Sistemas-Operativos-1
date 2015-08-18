#include <omp.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#define N_FILOSOFOS 5
#define ESPERA 100000

omp_lock_t tenedor[N_FILOSOFOS];

void pensar(int i)
{
  printf("Filosofo %d pensando...\n",i);
  usleep(random() % ESPERA);
}

void comer(int i)
{
  printf("Filosofo %d comiendo...\n",i);
  usleep(random() % ESPERA);
}

void tomar_tenedores(int i)
{
  /* Asumo el filosofo 0 es zurdo */
  if(i) {
    omp_set_lock(&tenedor[i]); /* Toma el tenedor a su derecha */
    omp_set_lock(&tenedor[(i+1)%N_FILOSOFOS]); /* Toma el tenedor a su izquierda */
  } else {
    omp_set_lock(&tenedor[(i+1)%N_FILOSOFOS]); /* Toma el tenedor a su izquierda */
    omp_set_lock(&tenedor[i]); /* Toma el tenedor a su derecha */
  }
}


void dejar_tenedores(int i)
{
  /* Asumo el filosofo 0 es zurdo */
  if(i) {
    omp_unset_lock(&tenedor[i]); /* Deja el tenedor de su derecha */
    omp_unset_lock(&tenedor[(i+1)%N_FILOSOFOS]); /* Deja el tenedor de su izquierda */
  } else {
    omp_unset_lock(&tenedor[(i+1)%N_FILOSOFOS]); /* Deja el tenedor de su izquierda */
    omp_unset_lock(&tenedor[i]); /* Deja el tenedor de su derecha */
  } 
}


void filosofo(int i)
{
  for (;;) {
    tomar_tenedores(i);
    comer(i);
    dejar_tenedores(i);
    pensar(i);
  }
}

int main ()
{
  int i;

  for (i=0;i<N_FILOSOFOS;i++)
    omp_init_lock(&tenedor[i]);
//  for (i=0;i<N_FILOSOFOS;i++)

  omp_set_num_threads(5);
  #pragma omp parallel sections
  {
    #pragma omp section
    { 
      filosofo(0);
    }     
    #pragma omp section
    { 
      filosofo(1);
    }     
    #pragma omp section
    { 
      filosofo(2);
    }     
    #pragma omp section
    { 
      filosofo(3);
    }     
    #pragma omp section
    { 
      filosofo(4);
    }     
  }

  //for (i=0;i<N_FILOSOFOS;i++)
  //  omp_destroy_lock(&tenedor[i]);

  return 0;
}
