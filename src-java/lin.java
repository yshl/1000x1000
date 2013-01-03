import java.lang.Math.*;

class Lin{
    static void swap(double[][] a, int i, int j)
    {
	double[] tmp=a[i];
	a[i]=a[j];
	a[j]=tmp;
    }
    static void swap(double[] a, int i, int j)
    {
	double tmp=a[i];
	a[i]=a[j];
	a[j]=tmp;
    }
    static void pivot(double[][] a, double[] b, int i)
    {
	int n=a.length;
	double ajimax=Math.abs(a[i][i]);
	int maxj=i;

	for(int j=i+1; j<n; j++){
	    double aji=Math.abs(a[j][i]);
	    if(aji>ajimax){
		ajimax=aji;
		maxj=j;
	    }
	}
	if(maxj!=i){
	    swap(a,i,maxj);
	    swap(b,i,maxj);
	}
    }
    public static void solve(double[][] a, double[] b)
    {
	int n=a.length;
	for(int i=0; i<n; i++){
	    double aijmax=Math.abs(a[i][0]);
	    for(int j=1; j<n; j++){
		double aij=Math.abs(a[i][j]);
		if(aij>aijmax){
		    aijmax=aij;
		}
	    }
	    double factor=1.0/aijmax;
	    for(int j=0; j<n; j++){
		a[i][j]*=factor;
	    }
	    b[i]*=factor;
	}
	for(int i=0; i<n; i++){
	    pivot(a,b,i);
	    double factor=1.0/a[i][i];
	    for(int j=i+1; j<n; j++){
		a[i][j]*=factor;
	    }
	    b[i]*=factor;
	    for(int j=i+1; j<n; j++){
		factor=a[j][i];
		for(int k=i+1; k<n; k++){
		    a[j][k]-=factor*a[i][k];
		}
		b[j]-=factor*b[i];
	    }
	}
	for(int i=n-1; i>=0; i--){
	    for(int j=i+1; j<n; j++){
		b[i]-=a[i][j]*b[j];
	    }
	}
    }
}
