#include<math.h>
#include"common_array.h"

double abs_max_array(double* x, int begin, int end)
{
    double xmax;
    int i;

    xmax=fabs(x[begin]);
    for(i=begin+1; i<end; i++){
	double xi=fabs(x[i]);
	if(xi>xmax){
	    xmax=xi;
	}
    }
    return xmax;
}

void scale_array(double* x, int begin, int end, double factor)
{
    int i;
    for(i=begin; i<end; i++){
	x[i]*=factor;
    }
}
