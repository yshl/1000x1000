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

static void axpy(int N, double y[N], double a, double x[N])
{
    int i;
    for(i=0; i<N; i++){
	y[i]+=a*x[i];
    }
}

static void qr_gramschmidt(int N, double a[N][N], double q[N][N], double r[N][N])
{
    int i,j;
    for(i=0; i<N; i++){
	for(j=0; j<N; j++){
	    r[i][j]=0.0;
	}
    }
    for(i=0; i<N; i++){
	double factor;
	for(j=0; j<N; j++){
	    q[i][j]=a[j][i];
	}
	for(j=0; j<i; j++){
	    r[j][i]=dot(N,q[j],q[i]);
	}
	for(j=0; j<i; j++){
	    axpy(N,q[i],-r[j][i],q[j]);
	}
	r[i][i]=norm(N,q[i]);
	factor=1.0/r[i][i];
	for(j=0; j<N; j++){
	    q[i][j]*=factor;
	}
    }
}

static void qmul_vec(int N, double b[N], double q[N][N])
{
    int i,j;
    double *tmp;
    tmp=malloc(sizeof(double)*N);
    if(tmp==NULL){
	perror("");
	exit(1);
    }
    for(i=0; i<N; i++){
	tmp[i]=0.0;
    }
    for(i=0; i<N; i++){
	for(j=0; j<N; j++){
	    tmp[i]+=q[i][j]*b[j];
	}
    }
    for(i=0; i<N; i++){
	b[i]=tmp[i];
    }
    free(tmp);
}


void solve(int N, double a[N][N], double b[N])
{
    int i,j;
    double (*q)[N];
    double (*r)[N];

    q=malloc(sizeof(double)*N*N);
    if(q==NULL){
	perror("");
	exit(1);
    }
    r=malloc(sizeof(double)*N*N);
    if(r==NULL){
	perror("");
	free(q);
	exit(1);
    }

    qr_gramschmidt(N,a,q,r);
    qmul_vec(N,b,q);

    for(i=N-1; i>=0; i--){
	for(j=i+1; j<N; j++){
	    b[i]-=r[i][j]*b[j];
	}
	b[i]/=r[i][i];
    }
    free(r);
    free(q);
}
