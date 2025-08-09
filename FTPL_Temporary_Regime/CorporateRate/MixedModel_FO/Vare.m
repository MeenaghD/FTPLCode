function [theta,error] = Vare(y,nlag,exo)
% Estimate VAR-X model 
%     Y = theta1*Y_lag + theta2*exo +e

%Inpute: 
%        y:  k*n matrix
%        nlag: number of lags.
%        exo: k*n matrix
 
%Output:
%        theta: (nlag*k)*k matrix  
%        error: k*n matrix, model residual

% Author: Yongdeng Xu
% E-mail: xuyongdeng@hotmail.com

        [k,T]=size(y);
        Y=y(:,nlag+1:T); 
        if nlag==0;
            X=[];
        elseif nlag==1;
           X=y(:,1:T-1);
        elseif nlag==2;
           Y1=y(:,1:T-nlag);
           Y2=y(:,2:T-nlag+1);
           X=[Y1;Y2];
        else
           X=[];
           for i=1:nlag
               X=[X;y(:,i:T-nlag+i-1)];
           end
        end
        if 1-isempty(exo);                          % including exogenous variabels
           X=[X;exo(:,nlag+1:T)];
        end;
        A=[Y;X]; A=A(:,find(sum(isnan(A),1)==0));    % delete NaN observations
        YY=A(1:k,:); XX=A(k+1:end,:);
        theta = YY*XX'*inv(XX*XX');     
        error=Y - theta*X;
        merror = NaN(k,1);
        error = [merror*ones(1,nlag) error];
  end


