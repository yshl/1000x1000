n=1000;
a=ones(n,n)+1000*eye(n);
b=1000*ones(n,1);
x=a\b;
disp(max(abs(x-0.5)));
