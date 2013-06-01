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
    static void scale_vector(double[] a, int from, int end, double factor)
    {
	for(int i=from; i<end; i++){
	    a[i]*=factor;
	}
    }
    static void scale_matrix_row(double[][] a, double[] b)
    {
	int n=a.length;
	for(int i=0; i<n; i++){
	    double aimax=Math.abs(a[i][0]);
	    for(int j=1; j<n; j++){
		double aij=Math.abs(a[i][j]);
		if(aij>aimax){
		    aimax=aij;
		}
	    }
	    double factor=1.0/aimax;
	    scale_vector(a[i],0,n,factor);
	    b[i]*=factor;
	}
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
    static void update_lower_col(double[][] a,double[] b,int i,int blockend)
    {
	int n=a.length;
	for(int i1=i; i1<blockend; i1++){
	    pivot(a,b,i1);
	    double factor=1.0/a[i1][i1];
	    scale_vector(a[i1],i1+1,blockend,factor);
	    b[i1]*=factor;
	    for(int j=i1+1; j<n; j++){
		factor=a[j][i1];
		for(int k=i1+1; k<blockend; k++){
		    a[j][k]-=factor*a[i1][k];
		}
		b[j]-=factor*b[i1];
	    }
	}
    }
    static void update_upper_row(double[][] a,int i,int blockend)
    {
	int n=a.length;
	for(int i1=i; i1<blockend; i1++){
	    scale_vector(a[i1],blockend,n,1.0/a[i1][i1]);
	    for(int j=i1+1; j<blockend; j++){
		for(int k=blockend; k<n; k++){
		    a[j][k]-=a[j][i1]*a[i1][k];
		}
	    }
	}
    }
    static void forward_elimination(double[][] a,int i,int blockend)
    {
	int n=a.length;
	int blocksize=4;
	for(int j=blockend; j<n; j++){
	    int l;
	    for(l=i; l+blocksize<=blockend; l+=blocksize){
		double ajl0=a[j][l+0];
		double ajl1=a[j][l+1];
		double ajl2=a[j][l+2];
		double ajl3=a[j][l+3];
		for(int k=blockend; k<n; k++){
		    a[j][k]-=ajl0*a[l+0][k]
			+ajl1*a[l+1][k]
			+ajl2*a[l+2][k]
			+ajl3*a[l+3][k];
		}
	    }
	    for(; l<blockend; l++){
		double ajl=a[j][l];
		for(int k=blockend; k<n; k++){
		    a[j][k]-=ajl*a[l][k];
		}
	    }
	}
    }
    static void back_substitution(double[][] a, double[] b)
    {
	int n=a.length;
	for(int i=n-1; i>=0; i--){
	    for(int j=i+1; j<n; j++){
		b[i]-=a[i][j]*b[j];
	    }
	}
    }
    public static void solve(double[][] a, double[] b)
    {
	int n=a.length;
	scale_matrix_row(a,b);
	int blocksize=16;
	for(int i=0; i<n; i+=blocksize){
	    int blockend=Math.min(i+blocksize,n);
	    update_lower_col(a,b,i,blockend);
	    update_upper_row(a,i,blockend);
	    forward_elimination(a,i,blockend);
	}
	back_substitution(a,b);
    }
}
