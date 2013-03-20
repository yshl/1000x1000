#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"lin_pointer.h"

#define swap(type, a, b) do{type tmp=a; a=b; b=tmp;}while(0)

static void scale_matrix_row(double** a, double* b, int N)
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

static void pivot(double** a, double* b, int N, int i)
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
	swap(double*, a[i], a[maxj]);
	swap(double, b[i], b[maxj]);
    }
}

static void update_upper_row(double** a, double* b, int N, int i)
{
    int j;
    double factor;
    factor=1.0/a[i][i];
    for(j=i+1; j<N; j++){
	a[i][j]*=factor;
    }
    b[i]*=factor;
}

static void forward_elimination(double** a, double* b, int N, int i)
{
    int j,k;
    for(j=i+1; j<N; j++){
	double factor=a[j][i];
	for(k=i+1; k<N; k++){
	    a[j][k]-=factor*a[i][k];
	}
	b[j]-=factor*b[i];
    }
}

static void back_substitution(double** a, double* b, int N)
{
    int i,j;
    for(i=N-1; i>=0; i--){
	for(j=i+1; j<N; j++){
	    b[i]-=a[i][j]*b[j];
	}
    }
}

void solve(double **a, double *b, int N)
{
    int i;
    /* scale */
    scale_matrix_row(a,b,N);
    for(i=0; i<N; i++){
	/* pivot */
	pivot(a,b,N,i);
	/* forward */
	update_upper_row(a,b,N,i);
        forward_elimination(a,b,N,i);
    }
    /* back */
    back_substitution(a,b,N);
}
