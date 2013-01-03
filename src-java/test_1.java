import java.io.*;
import java.lang.Math.*;

class Test_1{
    public static void main(String arg[])
    {
	int n=1000;
	double[][] a=new double[n][n];
	double[] b=new double[n];
	Lin l=new Lin();

	for(int i=0; i<n; i++){
	    for(int j=0; j<n; j++){
		a[i][j]=1.0;
	    }
	    a[i][i]=1001.0;
	    b[i]=1000.0;
	}
	l.solve(a,b);

	double maxerr=0.0;
	for(int i=0; i<n; i++){
	    if(Math.abs(b[i]-0.5)>Math.abs(maxerr)){
		maxerr=b[i]-0.5;
	    }
	}
	System.out.println(maxerr);
    }
}
