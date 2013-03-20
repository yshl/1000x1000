#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"lin_array.h"

static double norm(int N, double v[N])
{
    int i;
    double sum=0.0;
    for(i=0; i<N; i++){
	sum+=v[i]*v[i];
    }
    return sqrt(sum);
}

static double dot(int N, double u[N], double v[N])
{
    int i;
    double sum=0.0;
    for(i=0; i<N; i++){
	sum+=u[i]*v[i];
    }
    return sum;
}

void solve(int N, double a[N][N], double b[N])
{
    int i,j,k;
    double *u;
    u=malloc(sizeof(double)*N);
    if(u==NULL){
	perror("");
	exit(1);
    }
    for(i=0; i<N; i++){
	double factor, rii;
	rii=norm(N,a[i]);
	factor=1.0/rii;
	for(j=0; j<N; j++){
	    a[i][j]*=factor;
	}
	b[i]*=factor;
	for(j=i+1; j<N; j++){
	    double rij=dot(N,a[i],a[j]);
	    for(k=0; k<N; k++){
		a[j][k]-=rij*a[i][k];
	    }
	    b[j]-=rij*b[i];
	}
    }
    for(i=0; i<N; i++){
	u[i]=0.0;
    }
    for(j=0; j<N; j++){
	for(i=0; i<N; i++){
	    u[i]+=a[j][i]*b[j];
	}
    }
    for(i=0; i<N; i++){
	b[i]=u[i];
    }
    free(u);
}
