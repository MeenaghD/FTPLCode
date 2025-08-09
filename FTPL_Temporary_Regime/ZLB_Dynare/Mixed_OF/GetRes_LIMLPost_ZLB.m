function [residual,residual_dt,inno,rho_est,nst_inx] = GetRes_LIMLPost_ZLB(M_,endo,inx_expect,inx_eqs)
% This function contains three parts. 
% 1) generate lag and lead variables. The lead variables are generated
% Through LIML.
% 2) obtain the model residuals. We make use the dynare function
% "_dynamic". The idea is following: give all the varialbes in _dynamic
% their values, but the model residuals the values of zero. Then the lhs-rhs are
% the model residuals. 
% 3) estimate residual AR coefficients. Obtain rhos and innovations. 


%Input: 
%	fname:  fname= M_.fname;. 
%	act_data: k*T matrix 
%	ind_lead=[1:7,13:14];             % The variables you used to generate E_t[y_(t+1)] by LIML
%	ind_eq=[5 2 1 3 10 13 14];        % Select Equations that contains model residuals,
%Output:
%	Residual: is the structure residuals;  k*T matrix
%	Innovation: model innovations, exogenous variables ;  k*T matrix
%	rho_hat: estimated AR coefficients for structure residuals;
%	nst_inx: index of nonstationary shocks if there are.


% load([fname '_results.mat']);
lead_lag=M_.lead_lag_incidence;
[k,T]=size(endo);
vars=[endo;zeros(M_.endo_nbr-k,T)];
vars_lag=[NaN*vars(:,1) vars(:,1:end-1)];                % first observation is lost
vars_lead=vars;

if isempty(inx_expect)
    inx_expect=find(lead_lag(3,:)~=0);                   % find E_t[y_(t+1)] index
    [theta,error] = Vare(endo(inx_expect,:),1,[]);       % estimate E_t[y_(t+1)] by LIML
    endo_lead=theta*endo(inx_expect,:);
    vars_lead(inx_expect,:)=endo_lead;                   % updata var_lead
else
    [theta,error] = Vare(endo(inx_expect,:),1,[]);       % estimate E_t[y_(t+1)] by LIML
    endo_lead=theta*endo(inx_expect,:);
    vars_lead(inx_expect,:)=endo_lead;                   % updata var_lead
end

y=[vars_lag; vars; vars_lead];
temp=[lead_lag(1,:) lead_lag(2,:) lead_lag(3,:)];
y=y(find(temp~=0),:);
x=NaN(T,M_.exo_nbr);
residual=NaN(M_.endo_nbr,T);
for t=1:T;
    [residual(:,t), g1, g2, g3] = eval([ 'ftpl_zlb.dynamic(y(:,t), x, M_.params, 0, t)']);
end
  if isempty(inx_eqs)
     residual=residual(1:M_.exo_nbr,:);
  else
     residual=residual(inx_eqs,:);
  end
  
%  Below is estimate of AR coefficients. The shock process I used is AR(1)
%  You may change it to ARMA or AR(n)-X if you like.
  nst_inx=NaN(M_.exo_nbr,1);                                       % index of nonstatioary shocks
  for j=rows(residual):-1:1;
    [theta1,resid_dt]=Vare(residual(j,:),0, [ones(1,T); [1:T]]); % detrending in case there is a trend
    % g is regressed on g(-1) and productivity shock
    if j==1
        residual_dt(j,:)=resid_dt;
        [rho_tmp,inno(j,:)] = Vare(residual_dt(j,:),1,inno(5,:));
        rho_est_tmp(:,j)=rho_tmp(1,1);
        rho_gy(1,1)=rho_tmp(1,2);
    % shock4 is treated as non-stationary
    elseif j==5
        dfresid=diff(residual(j,:)');
        dfresid=[NaN dfresid'];
        [rho_est_tmp(:,j),inno(j,:)] = Vare(dfresid,1,[]);   
        nst_inx(j)=j;
    else
        residual_dt(j,:)=resid_dt;  
        [rho_est_tmp(:,j),inno(j,:)] = Vare(residual_dt(j,:),1,[]); 
    end

    if rho_est_tmp(:,j)>0.99999;
       rho_est_tmp(:,j)=0.9999;
    end         
  end

% Putting the coef on y(-1) in the g_shock equation as the second ar coefficient
rho_est(1)=rho_est_tmp(1,1);
rho_est(2)=rho_gy(1,1);
rho_est(3:size(rho_est_tmp,2)+1)=rho_est_tmp(2:end);

nst_inx=nst_inx(find(sum(isnan(nst_inx),2)==0),:);   % Delete NaN observations 
