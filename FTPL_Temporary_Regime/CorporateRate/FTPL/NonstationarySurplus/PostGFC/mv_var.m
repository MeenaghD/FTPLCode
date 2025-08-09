function [coeffs, resids, varcovar]=mv_var(data,nlags,const);

[n k]=size(data); if n > k; data=data'; end;
[n k]=size(data);

stoix=make_lagmatrix(data,nlags);

y=stoix(1:n,:);
if const==1;
    x=[ones(1,k-nlags);stoix(n+1:(nlags+1)*n,:)];
elseif const==0;
    x=stoix(n+1:(nlags+1)*n,:);
end;

% coeffs=y*x'*inv(x*x');
coeffs=y*x'/(x*x');

resids=y-coeffs*x;

varcovar=resids*resids'/(k-nlags);

