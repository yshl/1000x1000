#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"lin_array.h"

#define swap(type, a, b) do{type tmp=a; a=b; b=tmp;}while(0)

static void scale(int N, double a[N][N], double b[N], double scale_b[N])
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

static void pivot(int N, double a[N][N], int i, int *maxj, int *maxk)
{
    double ajimax=fabs(a[i][i]);
    int j, k;

    *maxj=i;
    *maxk=i;
    for(j=i; j<N; j++){
	for(k=i; k<N; k++){
	    double aji=fabs(a[j][k]);
	    if(aji>ajimax){
		ajimax=aji;
		*maxj=j;
		*maxk=k;
	    }
	}
    }
}

static void swap_pivot(int N, double a[N][N], double b[N],
	int i, int maxj, int maxk)
{
    int j;
    if(i!=maxj){
	for(j=0; j<N; j++){
	    swap(double, a[i][j], a[maxj][j]);
	}
	swap(double, b[i], b[maxj]);
    }
    if(i!=maxk){
	for(j=0; j<N; j++){
	    swap(double, a[j][i], a[j][maxk]);
	}
    }
}

void solve(int N, double a[N][N], double b[N])
{
    int i,j,k,maxj,maxk;
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
    pivot(N, a, 0, &maxj, &maxk);
    for(i=0; i<N; i++){
	double factor, ajkmax;
	/* pivot */
	swap_col[i]=maxk;
	swap_pivot(N, a, b, i, maxj, maxk);
	/* forward */
	factor=1.0/a[i][i];
	for(j=i+1; j<N; j++){
	    a[i][j]*=factor;
	}
	b[i]*=factor;

	ajkmax=0.0;
	maxj=i+1;
	maxk=i+1;
	for(j=i+1; j<N; j++){
	    factor=a[j][i];
	    for(k=i+1; k<N; k++){
		double ajk;
		a[j][k]-=factor*a[i][k];
		ajk=fabs(a[j][k]);
		if(ajk>ajkmax){
		    ajkmax=ajk;
		    maxj=j;
		    maxk=k;
		}
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
