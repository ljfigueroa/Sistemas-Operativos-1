#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define N 1024
int A[N][N],B[N][N],C[N][N];

void dumpMatrix(int A[N][N])
{
  for (int i=0;i<N;i++) {
    for (int j=0;j<N;j++) {
      printf("%d ",A[i][j]);
    }
    printf("\n");
  }
  printf("*******************************************************\n");
}

int main()
{
  for (int i=0;i<N;i++)
    for (int j=0;j<N;j++) {
      A[i][j]=i + 1000*j;
      B[i][j]=j + i*1000;
    }
  
  #pragma omp parallel for 
    for (int k=0;k<N;k++) 
      for (int j=0;j<N;j++) 
        for (int i=0;i<N;i++) 
          C[k][i]+=A[k][j]*B[j][i];
//  dumpMatrix(C);
  return 0;
}

