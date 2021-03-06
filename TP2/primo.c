#include <stdio.h>
#include <omp.h>

int es_primo=1;
int main(int argc, char **argv)
{
  int h,b,s;
  double start, end; 
  long long i,n;
  if (argc<2) 
    return -1;
  sscanf(argv[1], "%lld", &n);

  start = omp_get_wtime();

  #pragma omp parallel
  { 
    h = omp_get_thread_num();
    b = 2 + h; 
    s = omp_get_num_threads(); 
    for (i=b; i<n && es_primo; i+=s) {
      if (n % i == 0)
      {
         es_primo=0;
      } 
    }
  }
  end = omp_get_wtime();
  
  printf("El numero %lld %s primo.\n",n, es_primo ? "es" : "no es");
  printf("Timpo %f \n", end-start);
  return 0;
}
