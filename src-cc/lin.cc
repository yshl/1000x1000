using namespace std;
#include<iostream>
#include<cmath>
#include<vector>
#include"lin.h"

template<typename T1> void swp(T1 &a, T1 &b)
{
    T1 tmp=a;
    a=b;
    b=tmp;
}

static void pivot(vector<vector<double> > & a, vector<double> &b, size_t i)
{
    double ajimax=fabs(a[i][i]);
    size_t maxj=i;
    size_t N=a.size();

    for(size_t j=i+1; j<N; j++){
	double aji=fabs(a[j][i]);
	if(aji>ajimax){
	    ajimax=aji;
	    maxj=j;
	}
    }
    if(i!=maxj){
	swp(a[i],a[maxj]);
	swp(b[i],b[maxj]);
    }
}

void solve(vector<vector<double> > &a, vector<double> &b)
{
    size_t N=a.size();

    /* scale */
    for(size_t i=0; i<N; i++){
	double aijmax=fabs(a[i][0]);
	for(size_t j=0; j<N; j++){
	    double aij=fabs(a[i][j]);
	    if(aij>aijmax){
		aijmax=aij;
	    }
	}
	double factor=1.0/aijmax;
	for(size_t j=0; j<N; j++){
	    a[i][j]*=factor;
	}
	b[i]*=factor;
    }
    for(size_t i=0; i<N; i++){
	/* pivot */
	pivot(a,b,i);
	/* forward */
	double factor=1.0/a[i][i];
	for(size_t j=i+1; j<N; j++){
	    a[i][j]*=factor;
	}
	b[i]*=factor;
	for(size_t j=i+1; j<N; j++){
	    factor=a[j][i];
	    for(size_t k=i+1; k<N; k++){
		a[j][k]-=factor*a[i][k];
	    }
	    b[j]-=factor*b[i];
	}
    }
    /* back */
    for(size_t i=N; i>0;){
	i--;
	for(size_t j=0; j<i; j++){
	    b[j]-=a[j][i]*b[i];
	}
    }
}
