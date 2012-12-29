n=1000
a=matrix(1.0,nrow=n,ncol=n)+diag(1000.0,nrow=n,ncol=n)
b=matrix(1000.0,nrow=n,ncol=1)
x=solve(a,b)
print(max(abs(x-0.5)))
