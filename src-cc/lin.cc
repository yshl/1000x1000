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

static double abs_max_vector(vector<double> &v, size_t from, size_t end)
{
    double vmax=fabs(v[from]);
    for(size_t i=from+1; i<end; i++){
	vmax=max(vmax, fabs(v[i]));
    }
    return vmax;
}

static void scale_vector(vector<double> &v, size_t from, size_t end, double factor)
{
    for(size_t i=from; i<end; i++){
	v[i]*=factor;
    }
}

static void scale_matrix_row(vector<vector<double> > &a, vector<double> &b)
{
    const size_t N=a.size();
    for(size_t i=0; i<N; i++){
	double factor=1.0/abs_max_vector(a[i],0,N);
	scale_vector(a[i],0,N,factor);
	b[i]*=factor;
    }
}

static void pivot(vector<vector<double> > & a, vector<double> &b, size_t i)
{
    double ajimax=fabs(a[i][i]);
    size_t maxj=i;
    const size_t N=a.size();

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

static void update_lower_col(vector<vector<double> > &a, vector<double> &b,
	size_t i, size_t blockend)
{
    const size_t N=a.size();
    for(size_t i1=i; i1<blockend; i1++){
	pivot(a,b,i1);
	double factor=1.0/a[i1][i1];
	scale_vector(a[i1],i1+1,blockend,factor);
	b[i1]*=factor;

	for(size_t j=i1+1; j<N; j++){
	    factor=a[j][i1];
	    for(size_t k=i1+1; k<blockend; k++){
		a[j][k]-=factor*a[i1][k];
	    }
	    b[j]-=factor*b[i1];
	}
    }
}

static void update_upper_row(vector<vector<double> > &a, size_t i,
	size_t blockend)
{
    const size_t N=a.size();

    for(size_t i1=i; i1<blockend; i1++){
	scale_vector(a[i1],blockend,N,1.0/a[i1][i1]);
	for(size_t j=i1+1; j<blockend; j++){
	    for(size_t k=blockend; k<N; k++){
		a[j][k]-=a[j][i1]*a[i1][k];
	    }
	}
    }
}

static void forward_elimination(vector<vector<double> >&a, size_t i,
	size_t blockend)
{
    const size_t blocksize=48;
    const size_t N=a.size();

    for(size_t k=blockend; k<N; k+=blocksize){
	size_t kend=min(k+blocksize,N);
	for(size_t j=blockend; j<N; j++){
	    for(size_t l=i; l<blockend; l++){
		for(size_t k1=k; k1<kend; k1++){
		    a[j][k1]-=a[j][l]*a[l][k1];
		}
	    }
	}
    }
}

static void back_substitution(vector<vector<double> >&a, vector<double> &b)
{
    const size_t N=a.size();

    for(size_t i=N; i>0;){
	i--;
	for(size_t j=i+1; j<N; j++){
	    b[i]-=a[i][j]*b[j];
	}
    }
}

void solve(vector<vector<double> > &a, vector<double> &b)
{
    const size_t blocksize=48;
    const size_t N=a.size();

    /* scale */
    scale_matrix_row(a,b);
    for(size_t i=0; i<N; i+=blocksize){
	size_t blockend=min(i+blocksize, N);
	/* forward */
	update_lower_col(a,b,i,blockend);
	update_upper_row(a,i,blockend);
	forward_elimination(a,i,blockend);
    }
    /* back */
    back_substitution(a,b);
}
