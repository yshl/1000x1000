import std.stdio;
import std.math;
import lin_static;

void main()
{
    immutable int N=1000;
    auto a=new double[N][N];
    auto b=new double[N];

    foreach(i, ref ai; a){
	ai[]=1.0;
	ai[i]=1001.0;
    }
    b[]=1000.0;
    /* solve */
    solve!(N)(a,b);
    /* output */
    auto maxerr=0.0;
    foreach(bi; b){
	if(!(fabs(bi-0.5)<=fabs(maxerr))){
	    maxerr=bi-0.5;
	}
    }
    writef("%g\n", maxerr);
    if(!(fabs(maxerr)<=1.0e-8)){
	throw new Exception("Large error");
    }
}
