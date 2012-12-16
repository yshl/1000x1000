using namespace std;
#include<iostream>
#include<cmath>
#include<vector>
#include<cstdlib>
#include"lin.h"

int main()
{
    const size_t N=1000;
    vector<vector<double> > a(N);
    double maxerr;

    for(size_t i=0; i<N; i++){
	a[i].resize(N, 1.0);
	a[i][i]=1001.0;
    }
    vector<double> b(N, 1000.0);

    solve(a,b);

    /* output */
    maxerr=0.0;
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
