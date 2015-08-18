#include <stdio.h>
#include <omp.h>
#include <stdlib.h>

#define N_VISITANTES 100000
int visitantes = 0;
int N;

/* LAMPORT */
int *entering;
int *number;

/* max calcula el maximo en un arreglo de enteros */
int max(int *a, int size)
{
  int i;
  int max = a[0];
  for(i = 1; i < size; i++) 
    if(max < a[i])
      max = a[i];

  return max;
}

/* gana prioriza la ejecucion de los j threads que 
 * tienen mayor prioriadad de ejecucion que i
 */
int gana(int j, int i)
{
  return (number[j] < number[i]) || (number[i] == number[j] && j < i);
}

void lamport_lock(int i)
{
  int j;
  entering[i] = 1;
  asm("mfence");
  number[i] = max(number, N)+1;
  entering[i] = 0;
  asm("mfence");

  for(j = 0; j < N; j++) {
    while(entering[j])
      ;

    while (number[j] && gana(j,i))
      ;
  }
}

void lamport_unlock(int i)
{
  number[i] = 0;
}
 
/* FIN LAMPORT */

void molinete()
{
  int i;
  for (i = 0; i < N_VISITANTES; i++) {
    lamport_lock(omp_get_thread_num());
    visitantes++;
    lamport_unlock(omp_get_thread_num());
  }
}

int main ()
{
  N = 2;
  entering = calloc(N * sizeof (int), 1);
  number = calloc(N * sizeof (int), 1);

  #pragma omp parallel
  molinete();

  printf("Hoy hubo %d visitantes!\n", visitantes);
  return 0;
}
