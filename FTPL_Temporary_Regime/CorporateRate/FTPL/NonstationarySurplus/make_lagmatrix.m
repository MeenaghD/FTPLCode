function f=make_lagmatrix(y,hysterisis);
%This function creates a lagged matrix of a matrix y (pxT)
%where p is the number of the series and T is the number
%of observation. this function is compatible with the multiple
%equation estimation procedure.
[n k]=size(y);
steiles=k-hysterisis;
seires=hysterisis*n+n;
laggedy=zeros(seires,steiles);
for i=1:hysterisis+1;
    for j=1:n;
        laggedy(j+(i-1)*n,:)=y(j,(2+hysterisis-i):(k+1-i));
    end;
end;
f=laggedy;
