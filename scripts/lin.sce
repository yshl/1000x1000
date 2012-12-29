n=1000;
a=ones(n,n)+1000*eye(n,n);
b=1000*ones(n,1);
x=a\b;
print(%io(2),max(abs(x-0.5)));
quit;
