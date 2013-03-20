#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"lin_array.h"

void solve(int N, double a[N][N], double b[N])
{
    int i,j,k;
    
    for(i=0; i<N; i++){
	for(j=i+1; j<N; j++){
	    double c,s,factor;
	    double aik,ajk;
	    c=a[i][i];
	    s=a[j][i];
	    factor=1.0/hypot(c,s);
	    c*=factor;
	    s*=factor;
	    for(k=i; k<N; k++){
		aik=a[i][k];
		ajk=a[j][k];
		a[i][k]=c*aik+s*ajk;
		a[j][k]=-s*aik+c*ajk;
	    }
	    aik=b[i];
	    ajk=b[j];
	    b[i]=c*aik+s*ajk;
	    b[j]=-s*aik+c*ajk;
	}
    }
    for(i=N-1; i>=0; i--){
	for(j=i+1; j<N; j++){
	    b[i]-=a[i][j]*b[j];
	}
	b[i]/=a[i][i];
    }
}
