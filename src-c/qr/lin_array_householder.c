#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"lin_array.h"

static void mirror_vector(int N, double a[N][N], int i, double u[N])
{
    int j;
    double absai=0.0;

    for(j=i; j<N; j++){
	u[j]=a[j][i];
	absai+=u[j]*u[j];
    }
    absai=sqrt(absai);
    if(u[i]>=0.0){
	u[i]+=absai;
    }else{
	u[i]-=absai;
    }
}

void solve(int N, double a[N][N], double b[N])
{
    int i,j,k;
    double *u, *ua;
    
    u=malloc(sizeof(double)*N);
    if(u==NULL){
	perror("");
	exit(1);
    }
    ua=malloc(sizeof(double)*N);
    if(ua==NULL){
	perror("");
	free(u);
	exit(1);
    }

    for(i=0; i<N; i++){
	double factor, ub;

	mirror_vector(N,a,i,u);
	factor=0.0;
	for(j=i; j<N; j++){
	    factor+=u[j]*u[j];
	}
	if(factor==0.0){
	    continue;
	}
	factor=2.0/factor;
	for(j=i; j<N; j++){
	    ua[j]=0.0;
	}
	ub=0.0;
	for(k=i; k<N; k++){
	    for(j=i; j<N; j++){
		ua[j]+=u[k]*a[k][j];
	    }
	    ub+=u[k]*b[k];
	}
	for(j=i; j<N; j++){
	    double uj=factor*u[j];
	    for(k=i; k<N; k++){
		a[j][k]-=uj*ua[k];
	    }
	    b[j]-=factor*ub*u[j];
	}
    }
    for(i=N-1; i>=0; i--){
	for(j=i+1; j<N; j++){
	    b[i]-=a[i][j]*b[j];
	}
	b[i]/=a[i][i];
    }
    free(u);
    free(ua);
}
