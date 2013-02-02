#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"lin_array.h"

int main()
{
    const int N=1000;
    double (*a)[N];
    double *b;
    double maxerr;
    int i,j;

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
    /* matrix from http://www.cs.yale.edu/homes/spielman/BAP/lect6.pdf */
    for(i=0; i<N; i++){
	for(j=0; j<i; j++){
	    a[i][j]=-1.0;
	}
	a[i][i]=1.0;
	for(j=i+1; j<N-1; j++){
	    a[i][j]=0.0;
	}
	a[i][N-1]=1.0;

	b[i]=0.0;
	for(j=0; j<N; j++){
	    b[i]+=a[i][j];
	}
    }
    /* solve */
    solve(N,a,b);
    /* output */
    maxerr=0.0;
    for(i=0; i<N; i++){
	if(!(fabs(b[i]-1.0)<=fabs(maxerr))){
	    maxerr=b[i]-1.0;
	}
    }
    printf("%g\n",maxerr);
    if(!(fabs(maxerr)<=1.0e-8)){
	fprintf(stderr, "Large error\n");
	exit(1);
    }
    free(a);
    free(b);
    return 0;
}
