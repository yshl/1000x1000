using namespace std;
#include<iostream>
#include<cmath>
#include<cstdlib>
#include<boost/numeric/ublas/matrix.hpp>
#include<boost/numeric/ublas/vector.hpp>
#include<boost/numeric/ublas/lu.hpp>
using namespace boost::numeric::ublas;

int main()
{
    const size_t N=1000;
    matrix<double> a(N,N);
    boost::numeric::ublas::vector<double> b(N);
    permutation_matrix<> pm(N);

    for(size_t i=0; i<N; i++){
	for(size_t j=0; j<N; j++){
	    a(i,j)=1.0;
	}
	a(i,i)=1001.0;
	b[i]=1000.0;
    }

    lu_factorize(a,pm);
    lu_substitute(a,pm,b);

    /* output */
    double maxerr=0.0;
    for(size_t i=0; i<N; i++){
	if(fabs(b[i]-0.5)>maxerr){
	    maxerr=b[i]-0.5;
	}
    }
    cout << maxerr << endl;
    if(fabs(maxerr)>1.0e-8){
	cerr<<"Large error"<<endl;
	exit(1);
    }
    return 0;
}
