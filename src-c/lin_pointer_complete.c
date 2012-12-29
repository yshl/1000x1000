#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"lin_pointer.h"

#define swap(type, a, b) do{type tmp=a; a=b; b=tmp;}while(0)

static void scale(int N, double **a, double *b, double *scale_b)
{
    int i,j;

    for(i=0; i<N; i++){
	double aijmax=fabs(a[i][0]);
	double factor;
	for(j=1; j<N; j++){
	    aijmax=fmax(aijmax, fabs(a[i][j]));
	}
	factor=1.0/aijmax;
	for(j=0; j<N; j++){
	    a[i][j]*=factor;
	}
	b[i]*=factor;
    }
    for(j=0; j<N; j++){
	double aijmax=fabs(a[0][j]);
	double factor;
	for(i=1; i<N; i++){
	    aijmax=fmax(aijmax,fabs(a[i][j]));
	}
	factor=1.0/aijmax;
	for(i=0; i<N; i++){
	    a[i][j]*=factor;
	}
	scale_b[j]=factor;
    }
}

static int pivot(int N, double **a, double *b, int i)
{
    double ajimax=fabs(a[i][i]);
    int maxj=i, maxk=i, j, k;

    for(j=i; j<N; j++){
	for(k=i; k<N; k++){
	    double aji=fabs(a[j][k]);
	    if(aji>ajimax){
		ajimax=aji;
		maxj=j;
		maxk=k;
	    }
	}
    }
    if(i!=maxj){
	swap(double*, a[i], a[maxj]);
	swap(double, b[i], b[maxj]);
    }
    if(i!=maxk){
	for(j=0; j<N; j++){
	    swap(double, a[j][i], a[j][maxk]);
	}
    }
    return maxk;
}

void solve(double **a, double *b, int N)
{
    int i,j,k;
    double *scale_b;
    int *swap_col;

    scale_b=malloc(sizeof(double)*N);
    if(scale_b==NULL){
	perror("");
	exit(1);
    }
    swap_col=malloc(sizeof(double)*N);
    if(swap_col==NULL){
	perror("");
	free(scale_b);
	exit(1);
    }

    /* scale */
    scale(N, a, b, scale_b);
    for(i=0; i<N; i++){
	double factor;
	/* pivot */
	swap_col[i]=pivot(N,a,b,i);
	/* forward */
	factor=1.0/a[i][i];
	for(j=i+1; j<N; j++){
	    a[i][j]*=factor;
	}
	b[i]*=factor;
	for(j=i+1; j<N; j++){
	    factor=a[j][i];
	    for(k=i+1; k<N; k++){
		a[j][k]-=factor*a[i][k];
	    }
	    b[j]-=factor*b[i];
	}
    }
    /* back */
    for(i=N-1; i>=0; i--){
	for(j=i+1; j<N; j++){
	    b[i]-=a[i][j]*b[j];
	}
    }
    /* swap */
    for(i=N-1; i>=0; i--){
	if(swap_col[i]!=i){
	    int swc=swap_col[i];
	    swap(double, b[i], b[swc]);
	}
    }
    /* scale */
    for(i=0; i<N; i++){
	b[i]*=scale_b[i];
    }
    free(swap_col);
    free(scale_b);
}
