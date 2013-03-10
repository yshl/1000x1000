import std.stdio;
import std.math;

private void swap(T1)(ref T1 a, ref T1 b)
{
    auto tmp=a;
    a=b;
    b=tmp;
}

private void pivot(double[][] a, double[] b, size_t i)
{
    double ajimax=abs(a[i][i]);
    size_t maxj=i;
    foreach(j; i+1..a.length){
	double aji=abs(a[j][i]);
	if(aji>ajimax){
	    ajimax=aji;
	    maxj=j;
	}
    }
    if(i!=maxj){
	swap(a[i], a[maxj]);
	swap(b[i], b[maxj]);
    }
}

void solve(double[][] a, double[] b)
{
    /* scale */
    foreach(i, ref ai; a){
	double aijmax=0.0;
	foreach(aij; ai){
	    aijmax=fmax(aijmax,fabs(aij));
	}
	double factor=1.0/aijmax;
	ai[]*=factor;
	b[i]*=factor;
    }
    foreach(i; 0..a.length){
	/* pivot */
	pivot(a,b,i);
	/* forward */
	double factor=1.0/a[i][i];
	a[i][i+1..$]*=factor;
	b[i]*=factor;
	foreach(j; i+1..a.length){
	    factor=a[j][i];
	    a[j][i+1..$]-=factor*a[i][i+1..$];
	    b[j]-=factor*b[i];
	}
    }
    /* back */
    foreach_reverse(i, ai; a){
	foreach(j; i+1..ai.length){
	    b[i]-=ai[j]*b[j];
	}
    }
}
