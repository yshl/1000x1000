import std.stdio;
import std.math;
import std.algorithm;

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

private void update_lower_col(double[][] a, double[] b, size_t i, size_t blockend)
{
    foreach(i1; i..blockend){
	pivot(a,b,i1);
	double factor=1.0/a[i1][i1];
	a[i1][i1+1..blockend]*=factor;
	b[i1]*=factor;
	foreach(j; i1+1..a.length){
	    factor=a[j][i1];
	    a[j][i1+1..blockend]-=factor*a[i1][i1+1..blockend];
	    b[j]-=factor*b[i1];
	}
    }
}

private void update_upper_row(double[][] a, size_t i, size_t blockend)
{
    foreach(i1; i..blockend){
	double factor=1.0/a[i1][i1];
	a[i1][blockend..$]*=factor;
	foreach(j; i1+1..blockend){
	    a[j][blockend..$]-=a[j][i1]*a[i1][blockend..$];
	}
    }
}

private void forward_elimination(double[][] a, size_t i, size_t blockend)
{
    size_t n=a.length;
    size_t blocksize=32;
    for(size_t j=blockend; j<n; j+=blocksize){
	size_t jend=min(j+blocksize, n);
	for(size_t k=blockend; k<n; k+=blocksize){
	    size_t kend=min(k+blocksize, n);
	    foreach(j1; j..jend){
		foreach(l; i..blockend){
		    a[j1][k..kend]-=a[j1][l]*a[l][k..kend];
		}
	    }
	}
    }
}

void solve(double[][] a, double[] b)
{
    /* scale */
    foreach(i, ref ai; a){
	double factor=1.0/reduce!("fmax(a,fabs(b))")(0.0,ai);
	ai[]*=factor;
	b[i]*=factor;
    }
    /* forward */
    size_t n=a.length;
    size_t blocksize=32;
    for(size_t i=0; i<n; i+=blocksize){
	size_t blockend=min(i+blocksize,n);
	update_lower_col(a,b,i,blockend);
	update_upper_row(a,i,blockend);
	forward_elimination(a,i,blockend);
    }
    /* back */
    foreach_reverse(i, ai; a){
	foreach(j; i+1..ai.length){
	    b[i]-=ai[j]*b[j];
	}
    }
}
