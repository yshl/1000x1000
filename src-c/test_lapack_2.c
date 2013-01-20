#include<stdio.h>
#include<stdlib.h>
#include<math.h>
//#include<cblas.h>
//#include<clapack.h>

extern void dgetc2_(const int *, double *, const int *, int *, int *, int *);
extern void dgesc2_(const int *, double *, const int *, double *, int *, int *, double *);

int main()
{
    const int N=1000;
    double *a;
    double *b;
    double scale;
    int *ipiv, *jpiv;
    double maxerr;
    int i,j, info;

    a=malloc(sizeof(double)*N*N);
    if(a==NULL){
	perror("");
	exit(1);
    }
    b=malloc(sizeof(double)*N);
    if(b==NULL){
	perror("");
	exit(1);
    }
    ipiv=malloc(sizeof(int)*N);
    if(ipiv==NULL){
	perror("");
	exit(1);
    }
    jpiv=malloc(sizeof(int)*N);
    if(jpiv==NULL){
	perror("");
	exit(1);
    }
    /* matrix from http://www.cs.yale.edu/homes/spielman/BAP/lect6.pdf */
    for(i=0; i<N; i++){
	for(j=0; j<i; j++){
	    a[i+j*N]=-1.0;
	}
	a[i+i*N]=1.0;
	for(j=i+1; j<N-1; j++){
	    a[i+j*N]=0.0;
	}
	a[i+(N-1)*N]=1.0;
	b[i]=0.0;
	for(j=0; j<N; j++){
	    b[i]+=a[i+j*N];
	}
	ipiv[i]=0;
	jpiv[i]=0;
    }
    /* solve */
    dgetc2_(&N, a, &N, ipiv, jpiv, &info);
    dgesc2_(&N, a, &N, b, ipiv, jpiv, &scale);
    if(scale!=1.0){
	for(i=0; i<N; i++){
	    b[i]*=scale;
	}
    }
    /* output */
    maxerr=0.0;
    for(i=0; i<N; i++){
	if(fabs(b[i]-1.0)>fabs(maxerr)){
	    maxerr=b[i]-1.0;
	}
    }
    printf("%g\n",maxerr);
    if(fabs(maxerr)>1.0e-8){
	fprintf(stderr, "Large error\n");
	exit(1);
    }
    free(a);
    free(b);
    free(ipiv);
    free(jpiv);
    return 0;
}
