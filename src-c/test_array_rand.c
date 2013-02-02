#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<time.h>
#include"lin_array.h"

int main()
{
    const int N=1000;
    double (*a)[N];
    double *b, *x;
    double maxerr;
    int i,j;

    srand(clock());
    a=malloc(sizeof(double[N])*N);
    if(a==NULL){
	perror("");
	exit(1);
    }
    b=malloc(sizeof(double)*N);
    if(b==NULL){
	perror("");
	exit(1);
    }
    x=malloc(sizeof(double)*N);
    if(x==NULL){
	perror("");
	exit(1);
    }
    for(i=0; i<N; i++){
	for(j=0; j<N; j++){
	    a[i][j]=rand()/(double)RAND_MAX;
	}
	x[i]=rand()/(double)RAND_MAX;
    }
    for(i=0; i<N; i++){
	b[i]=0.0;
	for(j=0; j<N; j++){
	    b[i]+=a[i][j]*x[j];
	}
    }
    /* solve */
    solve(N,a,b);
    /* output */
    maxerr=0.0;
    for(i=0; i<N; i++){
	if(!(fabs(b[i]-x[i])<=fabs(maxerr))){
	    maxerr=b[i]-x[i];
	}
    }
    printf("%g\n",maxerr);
    if(!(fabs(maxerr)<=1.0e-8)){
	fprintf(stderr, "Large error\n");
	exit(1);
    }
    free(a);
    free(b);
    free(x);
    return 0;
}
