function [ pvalue, act_mmetric, Trans_Mdis_norm, mmetric,all_coeffs, act_coeffs, mean_var_sims] = Wald_nonstationary(act_data,boots_data,act_resid,boots_resid,rho_nonstat,var_order,var_variance)


% Calculate Wald statistic
% First we want to estimate the VAR on each simulation and use the
% coefficients, along with the variance of the simulation, to calculate the
% Wald statistic:
%                W=(beta-betabar)*inv(covar)*(beta-betabar)'


%Input: 
%   act_data: actural data         %  k*T matrix
%   boots_data: bootstraped data   %  k*T*nboot matrix
%	var_no=[1 2 3];                %  Choice of variables in the Wald calculation
%	var_order=1;                   %  Order of Var in the Wald caculation           
%	var_variance=1;                %  var_variance=1 ;including the variance of the endo variables
%	act_nonstatResid:              %  for actual data;
%	boots_nonstatResid             %  for bootstrap data;
%	rho_nonstat                    %  rhos

%Output:
%	Wald: Wald statistics
%	Trans_Mdis_norm: Transformed Wald
%   pvalue: p value of the test

[k, T, nboot]=size(boots_data);
[~, Tact]=size(act_data);
warning off
for j=1:nboot
    Y=boots_data(:,:,j);
    Y_resid=boots_resid(:,:,j);
    % errors_trend = zeros(size(Y_resid));
    % for i=2:T
    %     errors_trend(:,i)=Y_resid(:,i)+(rho_nonstat./(1-rho_nonstat)).*(Y_resid(:,i)-Y_resid(:,i-1));
    % end
    % 
    % [coef,error] = Vare(Y,var_order,[(lagmatrix(errors_trend',1))';ones(1,T); [1:T]]);
    
    [coef,error] = Vare(Y,var_order,[(lagmatrix(Y_resid',1))';ones(1,T); [1:T]]);

    coef = coef(1:k,1:var_order*k);
    error=error(:,find(sum(isnan(error),1)==0));   % Delete NaN observations
    if var_variance==0
        all_coeffs(j,:)=coef(:); 
    elseif var_variance==1
        var_sims=var(error');
        all_coeffs(j,:)=[coef(:); var_sims'];
    end
end

if var_variance==1
    mean_var_sims=mean(all_coeffs(:,10:12));
else
    mean_var_sims=[];
end

beta_bar=mean(all_coeffs);                      % Calculating beta_bar and (beta-beta_bar)
for j=1:nboot
    coef_dm(j,:)=all_coeffs(j,:)-beta_bar;
end
cov_beta=cov(coef_dm);                          % Calculating the covariance of beta-beta_bar

for j=1:nboot                                   % Calculating the Wald for each simulation.
    mmetric(j,1)=coef_dm(j,:)*inv(cov_beta)*coef_dm(j,:)';
end

% Calculate the Wald for the actual data.
% errors_trend = zeros(size(act_resid));
% for i=2:T
%     errors_trend(:,i)=act_resid(:,i)+(rho_nonstat./(1-rho_nonstat)).*(act_resid(:,i)-act_resid(:,i-1));
% end
% 
% [coef,act_data] = Vare(act_data,0,[1:T]);   % taking determinstic trend out 
% [coef,error] = Vare(act_data,var_order,[(lagmatrix(errors_trend',1))'; ones(1,T); [1:T]]);

[coef,error] = Vare(act_data,var_order,[(lagmatrix(act_resid',1))'; ones(1,Tact); [1:Tact]]);

coef = coef(1:k,1:var_order*k);

error=error(:,find(sum(isnan(error),1)==0));   % Delete NaN observations
if var_variance==0
    act_coeffs=coef(:);
elseif var_variance==1
    var_sims=var(error');
    act_coeffs=[coef(:); var_sims'];
end
act_coef_dm=act_coeffs'-beta_bar;
act_mmetric=act_coef_dm*inv(cov_beta)*act_coef_dm';

pvalue=sum(act_mmetric<mmetric)/nboot;


% This is the Transformed distance. This takes the Wald, which is
% chi-squared and converts it to a normal distribution, then calculated the
% t-statistic. The 95th percentile is 1.645, so a value of Trans_Mdis_norm
% less than this means the var_varianceel fits the data.
smmetric=sort(mmetric);
n95=round(nboot*0.95);
Trans_Mdis_norm=((sqrt(2*act_mmetric)-sqrt(2*size(act_coef_dm,2)-1))/(sqrt(2*smmetric(n95,1))-sqrt(2*size(act_coef_dm,2)-1)))*1.645;


end

