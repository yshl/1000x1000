#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#define swap(type, a, b) do{type tmp=a; a=b; b=tmp;}while(0)

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

static void solve(double **a, double *b, int N)
{
    int i,j,k;

    /* scale */
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
    for(i=0; i<N; i++){
	double factor;
	/* pivot */
	pivot(a,b,N,i);
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
}

int main()
{
    const int N=1000;
    double **a;
    double *b;
    int i,j;

    a=malloc(sizeof(double*)*N);
    if(a==NULL){
	perror("");
	exit(1);
    }
    for(i=0; i<N; i++){
	a[i]=malloc(sizeof(double)*N);
	if(a[i]==NULL){
	    perror("");
	    exit(1);
	}
    }
    b=malloc(sizeof(double)*N);
    if(b==NULL){
	perror("");
	exit(1);
    }
    for(i=0; i<N; i++){
	for(j=0; j<N; j++){
	    a[i][j]=1.0;
	}
	a[i][i]=1001.0;
	b[i]=1000.0;
    }
    /* solve */
    solve(a,b,N);
    /* output */
    for(i=0; i<N; i++){
	printf("%g\n",b[i]);
    }
    for(i=0; i<N; i++){
	free(a[i]);
    }
    free(a);
    free(b);
    return 0;
}
