#include <stdio.h>
#include <omp.h>

#define N_VISITANTES 1000000
int visitantes = 0;

void molinete()
{
  int i;
  for (i=0;i<N_VISITANTES;i++) {
    #pragma omp critical
    visitantes++;
  }
}

int main ()
{
  omp_set_num_threads(2); /* la ejecuto con 2 porque habia dos fc molinetes */
  #pragma omp parallel
  molinete();

  printf("Hoy hubo %d visitantes!\n", visitantes);
  return 0;
}
