using namespace std;
#include<iostream>
#include<cmath>
#include<vector>

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

int main()
{
    const size_t N=1000;
    vector<vector<double> > a(N);
    for(size_t i=0; i<N; i++){
	a[i].resize(N, 1.0);
	a[i][i]=1001.0;
    }
    vector<double> b(N, 1000.0);
    /* solve */
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
    /* output */
    for(size_t i=0; i<N; i++){
	cout << b[i] << endl;
    }
    return 0;
}
