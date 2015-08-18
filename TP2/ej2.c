#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define N 50000000

/* Encuentra el minimo de un arreglo de manera secuencial */
int find_min(int *arr, int size)
{
    int i;
    int min = arr[0];
    
    for(i=1; i < size; i++)
        if(arr[i] < min)
            min = arr[i];

    return min;
}

/* Separo en 5 thread el recorrido del arreglo */
int find_min_omp_range(int *arr, int size)
{
    int i, hilo, cond, minimo;
    int min[5] = {arr[0], arr[9999999], arr[19999999], 
		  arr[29999999], arr[39999999]};
		   
    omp_set_num_threads(5);
    #pragma omp parallel
    {
    hilo = omp_get_thread_num();
    i = (10000000*hilo);
    min[hilo] = find_min(arr+i, 10000000);

    }

    return find_min(min,5);
}

/* Primer intento de paralelizar el recorrido del arreglo,
 * es muy lenta.
 */
int find_min_omp_critical(int *arr, int size)
{
    int i;
    int min = arr[0];
    
    #pragma omp parallel for
    for(i=1; i < size; i++)
        #pragma omp critical
        if(arr[i] < min)
            min = arr[i];

    return min;
}

int *inicialize_arr_random(int size)
{
    int i;
    int *new_arr = malloc(sizeof(int)*size);
    
    srand(time(NULL));
    for(i = 0; i < size; i++)
        new_arr[i] = rand();

    return new_arr;
}

double time_( int (*f)(int *, int), int *arr, int size)
{
    double start, end;
    start = omp_get_wtime();
    f(arr,size);
    end = omp_get_wtime();
    return end-start;
}

int main()
{
    int *arr = inicialize_arr_random(N);

    printf("secuencial minimo: %d ,tiempo: %f \n",find_min(arr, N), time_(find_min,arr,N));
    printf("paralelo minimo: %d , tiempo: %f\n",find_min_omp_range(arr, N), time_(find_min_omp_range,arr,N));

    return 0;
}


