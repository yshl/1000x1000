#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"common_array.h"
#include"lin_pointer.h"

static int imin(int a, int b)
{
    return a<b?a:b;
}

static void scale_matrix_row(double** a, double* b, int N)
{
    int i;
    for(i=0; i<N; i++){
	double factor=1.0/abs_max_array(a[i],0,N);
	scale_array(a[i],0,N,factor);
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

static void update_lower_col(double** a, double* b, int N, int i,
	int blockend)
{
    int i1,j,k;
    for(i1=i; i1<blockend; i1++){
	double factor;
	/* pivot */
	pivot(a,b,N,i1);

	factor=1.0/a[i1][i1];
	scale_array(a[i1],i1+1,blockend,factor);
	b[i1]*=factor;

	for(j=i1+1; j<N; j++){
	    factor=a[j][i1];
	    for(k=i1+1; k<blockend; k++){
		a[j][k]-=factor*a[i1][k];
	    }
	    b[j]-=factor*b[i1];
	}
    }
}

static void update_upper_row(double** a, int N, int i, int blockend)
{
    int j,k,i1;
    for(i1=i; i1<blockend; i1++){
	double factor=1.0/a[i1][i1];
	scale_array(a[i1],blockend,N,factor);
	for(j=i1+1; j<blockend; j++){
	    for(k=blockend; k<N; k++){
		a[j][k]-=a[j][i1]*a[i1][k];
	    }
	}
    }
}

static void forward_elimination(double** a, int N, int i, int blockend)
{
    int j,k,l;
    const int blocksize=8;
    for(j=blockend; j<N; j++){
	for(l=i; l+blocksize<=blockend; l+=blocksize){
	    int l1;
	    double ajl[blocksize];
	    for(l1=0; l1<blocksize; l1++){
		ajl[l1]=a[j][l+l1];
	    }
	    for(k=blockend; k<N; k++){
		double sum=0.0;
		for(l1=0; l1<blocksize; l1++){
		    sum+=ajl[l1]*a[l+l1][k];
		}
		a[j][k]-=sum;
	    }
	}
	for(; l<blockend; l++){
	    double ajl=a[j][l];
	    for(k=blockend; k<N; k++){
		a[j][k]-=ajl*a[l][k];
	    }
	}
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
    int blocksize=48;
    /* scale */
    scale_matrix_row(a,b,N);
    for(i=0; i<N; i+=blocksize){
	int blockend=imin(i+blocksize,N);
	/* forward */
	update_lower_col(a,b,N,i,blockend);
	update_upper_row(a,N,i,blockend);
	forward_elimination(a,N,i,blockend);
    }
    /* back */
    back_substitution(a,b,N);
}
