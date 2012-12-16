#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"lin_pointer.h"

int main()
{
    const int N=1000;
    double **a;
    double *b;
    double maxerr;
    int i,j;

    a=malloc(sizeof(double*)*N);
    if(a==NULL){
	perror("");
	exit(1);
    }
    for(i=0; i<N; i++){
	a[i]=malloc(sizeof(double)*N);
	if(a[i]==NULL){
	    perror("");
	    exit(1);
	}
    }
    b=malloc(sizeof(double)*N);
    if(b==NULL){
	perror("");
	exit(1);
    }
    for(i=0; i<N; i++){
	for(j=0; j<N; j++){
	    a[i][j]=1.0;
	}
	a[i][i]=1001.0;
	b[i]=1000.0;
    }
    /* solve */
    solve(a,b,N);
    /* output */
    maxerr=0.0;
    for(i=0; i<N; i++){
	if(fabs(b[i]-0.5)>fabs(maxerr)){
	    maxerr=b[i]-0.5;
	}
    }
    printf("%g\n",maxerr);
    if(fabs(maxerr)>1.0e-8){
	fprintf(stderr,"Large error\n");
	exit(1);
    }

    for(i=0; i<N; i++){
	free(a[i]);
    }
    free(a);
    free(b);
    return 0;
}
