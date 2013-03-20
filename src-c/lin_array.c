#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"lin_array.h"

#define swap(type, a, b) do{type tmp=a; a=b; b=tmp;}while(0)

static void scale_matrix_row(int N, double a[N][N], double b[N])
{
    int i,j;
    for(i=0; i<N; i++){
	double aijmax=fabs(a[i][0]);
	double factor;
	for(j=1; j<N; j++){
	    double aij=fabs(a[i][j]);
	    if(aij>aijmax){
		aijmax=aij;
	    }
	}
	factor=1.0/aijmax;
	for(j=0; j<N; j++){
	    a[i][j]*=factor;
	}
	b[i]*=factor;
    }
}

static void pivot(int N, double a[N][N], double b[N], int i)
{
    double ajimax=fabs(a[i][i]);
    int maxj=i, j;

    for(j=i+1; j<N; j++){
	double aji=fabs(a[j][i]);
	if(aji>ajimax){
	    ajimax=aji;
	    maxj=j;
	}
    }
    if(i!=maxj){
	for(j=0; j<N; j++){
	    swap(double, a[i][j], a[maxj][j]);
	}
	swap(double, b[i], b[maxj]);
    }
}

static void update_upper_row(int N, double a[N][N], double b[N], int i)
{
    int j;
    double factor;
    factor=1.0/a[i][i];
    for(j=i+1; j<N; j++){
	a[i][j]*=factor;
    }
    b[i]*=factor;
}

static void forward_elimination(int N, double a[N][N], double b[N], int i)
{
    int j,k;
    for(j=i+1; j<N; j++){
	double factor;
	factor=a[j][i];
	for(k=i+1; k<N; k++){
	    a[j][k]-=factor*a[i][k];
	}
	b[j]-=factor*b[i];
    }
}

static void backward_substitution(int N, double a[N][N], double b[N])
{
    int i,j;
    for(i=N-1; i>=0; i--){
	for(j=i+1; j<N; j++){
	    b[i]-=a[i][j]*b[j];
	}
    }
}

void solve(int N, double a[N][N], double b[N])
{
    int i;
    /* scale */
    scale_matrix_row(N,a,b);
    for(i=0; i<N; i++){
	/* pivot */
	pivot(N,a,b,i);
	/* forward */
	update_upper_row(N,a,b,i);
	forward_elimination(N,a,b,i);
    }
    /* back */
    backward_substitution(N,a,b);
}
