#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<cblas.h>
#include<clapack.h>

int main()
{
    const int N=1000;
    double *a;
    double *b;
    int *ipiv;
    double maxerr;
    int i,j;

    a=malloc(sizeof(double)*N*N);
    if(a==NULL){
	perror("");
	exit(1);
    }
    b=malloc(sizeof(double)*N);
    if(b==NULL){
	perror("");
	exit(1);
    }
    ipiv=malloc(sizeof(int)*N);
    if(ipiv==NULL){
	perror("");
	exit(1);
    }
    for(i=0; i<N; i++){
	for(j=0; j<N; j++){
	    a[i*N+j]=1.0;
	}
	a[i*N+i]=1001.0;
	b[i]=1000.0;
	ipiv[i]=0;
    }
    /* solve */
    clapack_dgesv(CblasRowMajor, N, 1, a, N, ipiv, b, N);
    /* output */
    maxerr=0.0;
    for(i=0; i<N; i++){
	if(!(fabs(b[i]-0.5)<=fabs(maxerr))){
	    maxerr=b[i]-0.5;
	}
    }
    printf("%g\n",maxerr);
    if(!(fabs(maxerr)<=1.0e-8)){
	fprintf(stderr, "Large error\n");
	exit(1);
    }
    free(a);
    free(b);
    free(ipiv);
    return 0;
}
