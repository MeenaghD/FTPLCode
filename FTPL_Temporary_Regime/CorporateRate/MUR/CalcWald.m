function [nsims, pvalue_novar, Wald_novar, Trans_Wald_novar, mean_var_sims, pcFTPLweight1, mean_FTPLweight] = CalcWald(data,coef);
try

% initiate NK weights using average inflation data
pibar=mean(data(:,5));
coef(16)=exp(-coef(21)*abs(pibar));
coef(17)=exp(-coef(22)*abs(pibar));

save IIcoef coef;
load rho_est.mat;
% dynare orthodox.mod noclearall nopreprocessoroutput notime;    
[M_, oo_] = driver_function(coef,rho_est);

% Expected variables: c inv lab inf pk w rk
inx_expect=[7 2 9 5 3 6 10];        % or inx_expect=[];  The variables you used to generate E_t[y_(t+1)] used by LIML

% inx_eqs=[9 1 2 5 10 12 6 7 13 15 17];    % Select Equations that contains model residuals, in the order that you specify exogeneous variables in .mod
inx_eqs=[11 3 4 7 12 14 8 9 15 17 19];    % Select Equations that contains model residuals, (DAVID: CHECK EQUATIONS IN "+modfile/dynamic_resid.m")

act_data=data';                                       % act_data is a k*T matrix

% Step1:
% load orthodox/Output/orthodox_results.mat
% % [residual,inno,rho_est] = GetRes_Exact(fname,act_data,[]);                         % Exact method
%%
[residual,residual_dt, inno,rho_est,nst_inx] = GetRes_LIML(M_,act_data,inx_expect,inx_eqs);   % LIML

% Surplus is an ARIMA(1,1,0), do this separately
dsurplus=diff(data(:,32))';
[rho_surp, surp_inno] = Vare(dsurplus,1,[]);   
rho_est(13)=rho_surp;
inno(12,:)=[NaN surp_inno];
save resid_inno residual residual_dt inno
save rho_est rho_est
warning off
% dynare orthodox.mod noclearall nopreprocessoroutput notime;    
[M_, oo_] = driver_function(coef,rho_est);
warning on
%%
% Step2:
% Boostrap data by function Boots_data();
nvars=size(oo_.dr.ghx,1);

load start_lagged_pinf.mat
nboot=500; 

load NBbase.mat
load nomgdp_base.mat
% Running the code in serial was slow because it takes 20 seconds per simulation
% [boots_data, NKweight_output] = Boots_data_endog_switching(act_data,nvars,inno,nboot,start_lagged_pinf,coef);

% Running the code in parallel can use 8 cores. 
% You can't load the coefficients within the parfor loop.
% To do this we need to change the FILENAME.driver into a function that has the coef and rho_est as input that returns oo_.
% Also, I commented out the code that save the results and writes error messages at the end of the file. 
% Save this file as driver_function in the current directory, that is used in Boots_data_endog_switching_parallel
%%
[boots_data, NKweight_output,nsims,NB_Tall,debtgdp2_all,all_coef25] = Boots_data_endog_switching_parallel_orthodox_weighted(act_data,nvars,inno,nboot,start_lagged_pinf,coef,rho_est,NBbase,nomgdp);
pcFTPLweight1=sum(sum(all_coef25==1))/(size(boots_data,3)*size(boots_data,2));
mean_FTPLweight=mean(mean(all_coef25));

%%   
if nsims < 300
    pvalue_novar=0;
    Wald_novar=3000;
    Trans_Wald_novar=3000;
    mean_var_sims=[3000 3000 3000];
else
% Step3:
% Calculate Wald statistics and transformed t statistics by function II_Wald()
var_variance=1;         %  var_variance=0 ; not including the variance of the shocks in the Wald calculation
                        %  var_variance=1 ;including the variance
var_order=1;            %  Order of var in the Wald caculation           
var_no=[8 1 5];            %  y, pinf, r
nst_inx=[5];    % List of residuals that are non-stationary

act_nonstatResid=residual(nst_inx,:);
rho_nonstat=rho_est(nst_inx+1); % nst_inx+1 because we have the coefficient on productivity in the gov shock equation
boots_inx=M_.orig_endo_nbr - M_.exo_nbr + nst_inx;
boots_nonstatResid = boots_data(boots_inx,:,:);
 
[pvalue_novar, Wald_novar, Trans_Wald_novar, all_coeffs_novar, act_coeffs_novar, mean_var_sims_novar] = Wald_nonstationary(act_data(var_no,:),boots_data(var_no,:,:),act_nonstatResid,boots_nonstatResid,rho_nonstat,var_order,0);
% delete([fname '.log'],[fname '.m'],[fname '_static.m'],[fname '_set_auxiliary_variables.m']);
end
catch
    nsims=0;
    pvalue_novar=0;
    Wald_novar=1000;
    Trans_Wald_novar=1000;
    mean_var_sims=[1000 1000 1000];
end

end