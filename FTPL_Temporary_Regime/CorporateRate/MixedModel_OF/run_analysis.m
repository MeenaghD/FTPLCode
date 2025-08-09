dataAll=load('USdata_1959_2017.mat');
dataPre=load('USdata_1959_2007.mat');
dataPost=load('USdata_2008_2017.mat');
load coef_best_Orthodox_30pc_10pc.mat

dataAll=dataAll.data;
dataPre=dataPre.data;
dataPost=dataPost.data;
%%
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% Orthodox Model for PreGFC
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

% initiate NK weights using average inflation data
pibarPre=mean(dataPre(:,5).^2);
coefPre(16)=exp(-coefPre(21)*abs(pibarPre));
coefPre(17)=exp(-coefPre(22)*abs(pibarPre));

save IIcoefPre coefPre;
load rho_estPre.mat;
rho_estPre=rho_estPre;
% dynare orthodox.mod noclearall nopreprocessoroutput notime;    
[M_Pre, oo_Pre] = driver_function_orthodox(coefPre,rho_estPre);

% Expected variables: c inv lab inf pk w rk
inx_expect=[7 2 9 5 3 6 10];        % or inx_expect=[];  The variables you used to generate E_t[y_(t+1)] used by LIML

% inx_eqs=[9 1 2 5 10 12 6 7 13 15 17];    % Select Equations that contains model residuals, in the order that you specify exogeneous variables in .mod
inx_eqs=[11 3 4 7 12 14 8 9 15 17 19];    % Select Equations that contains model residuals, (DAVID: CHECK EQUATIONS IN "+modfile/dynamic_resid.m")

act_dataPre=dataPre';                                       % act_data is a k*T matrix

% Step1:
% load orthodox/Output/orthodox_results.mat
% % [residual,inno,rho_est] = GetRes_Exact(fname,act_data,[]);                         % Exact method

[residualPre,residualPre_dt, innoPre,rho_estPre,nst_inxPre] = GetRes_LIMLPre(M_Pre,act_dataPre,inx_expect,inx_eqs);   % LIML
save resid_innoPre residualPre residualPre_dt innoPre
save rho_estPre rho_estPre
warning off
% dynare orthodox.mod noclearall nopreprocessoroutput notime;    
[M_Pre, oo_Post] = driver_function_orthodox(coefPre,rho_estPre);
warning on
nvarsPre=size(oo_Pre.dr.ghx,1);

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
%%
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% FTPL Model for PostGFC
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% initiate NK weights using average inflation data
pibarPost=mean(dataPost(:,5));
coefPost(16)=exp(-coefPost(21)*abs(pibarPost));
coefPost(17)=exp(-coefPost(22)*abs(pibarPost));

save IIcoefPost coefPost;
load rho_estPost.mat;
rho_estPost=rho_estPost;
% dynare orthodox.mod noclearall nopreprocessoroutput notime;    
[M_Post, oo_Post] = driver_function_ftpl(coefPost,rho_estPost);

% Expected variables: c inv lab inf pk w rk
inx_expect=[7 2 9 5 3 6 10];        % or inx_expect=[];  The variables you used to generate E_t[y_(t+1)] used by LIML

% inx_eqs=[9 1 2 5 10 12 6 7 13 15 17];    % Select Equations that contains model residuals, in the order that you specify exogeneous variables in .mod
inx_eqs=[11 3 4 7 12 14 8 9 15 17 19];    % Select Equations that contains model residuals, (DAVID: CHECK EQUATIONS IN "+modfile/dynamic_resid.m")

act_dataPost=dataPost';                                       % act_data is a k*T matrix

% Step1:
% load orthodox/Output/orthodox_results.mat
% % [residual,inno,rho_est] = GetRes_Exact(fname,act_data,[]);                         % Exact method

[residualPost,residualPost_dt, innoPost,rho_estPost,nst_inxPost] = GetRes_LIMLPost(M_Post,act_dataPost,inx_expect,inx_eqs);   % LIML

% Surplus is an ARIMA(1,1,0), do this separately
dsurplus=diff(dataPost(:,32))';
[rho_surp, surp_inno] = Vare(dsurplus,1,[]);   
rho_estPost(13)=rho_surp;
innoPost(12,:)=[NaN surp_inno];
save resid_innoPost residualPost residualPost_dt innoPost
save rho_estPost rho_estPost
warning off
% dynare orthodox.mod noclearall nopreprocessoroutput notime;    
[M_Post, oo_Post] = driver_function_ftpl(coefPost,rho_estPost);
warning on
nvarsPost=size(oo_Post.dr.ghx,1);

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

%%
% Step2:
% Boostrap data by function Boots_data();

load start_lagged_pinf.mat
nboot=500; 
load NBbase.mat

%%
% Running the code in serial was slow because it takes 20 seconds per simulation
% [boots_data, NKweight_output] = Boots_data_endog_switching(act_data,nvars,inno,nboot,start_lagged_pinf,coef);

% Running the code in parallel can use 8 cores. 
% You can't load the coefficients within the parfor loop.
% To do this we need to change the FILENAME.driver into a function that has the coef and rho_est as input that returns oo_.
% Also, I commented out the code that save the results and writes error messages at the end of the file. 
% Save this file as driver_function in the current directory, that is used in Boots_data_endog_switching_parallel
[boots_data, NKweight_output] = Boots_data_endog_switching_parallel_mix(act_dataPre,act_dataPost,nvarsPre,nvarsPost,innoPre,innoPost,nboot,start_lagged_pinf,coefPre,coefPost,rho_estPre,rho_estPost,NBbase);
%%   
% Step3:
% Calculate Wald statistics and transformed t statistics by function II_Wald()
var_variance=1;         %  var_variance=0 ; not including the variance of the shocks in the Wald calculation
                        %  var_variance=1 ;including the variance
var_order=1;            %  Order of var in the Wald caculation           
var_no=[8 1 5];            %  y, pinf, r
nst_inx=[5];    % List of residuals that are non-stationary

act_nonstatResid=[residualPre(nst_inx,:) residualPost(nst_inx,:)];
rho_nonstat=(rho_estPre(nst_inx+1)+rho_estPost(nst_inx+1))/2; % nst_inx+1 because we have the coefficient on productivity in the gov shock equation
boots_inx=M_Post.orig_endo_nbr - M_Post.exo_nbr + nst_inx;
boots_nonstatResid = boots_data(boots_inx,:,:);
act_dataAll=dataAll';

[pvalue, Wald, Trans_Wald, mmetric, all_coeffs, act_coeffs, mean_var_sims] = Wald_nonstationary(act_dataAll(var_no,:),boots_data(var_no,:,:),act_nonstatResid,boots_nonstatResid,rho_nonstat,var_order,var_variance);
[pvalue_novar, Wald_novar, Trans_Wald_novar, all_coeffs_novar, act_coeffs_novar, mean_var_sims_novar] = Wald_nonstationary(act_dataAll(var_no,:),boots_data(var_no,:,:),act_nonstatResid,boots_nonstatResid,rho_nonstat,var_order,0);

%%
% % Auxiliary bounds
[nsims, nvar]=size(all_coeffs);

for i=1:nvar
    s_all_coeffs(:,i)=sort(all_coeffs(:,i));
end

lower=s_all_coeffs(round(0.025*nsims),:);
upper=s_all_coeffs(round(0.975*nsims),:);
average=mean(s_all_coeffs);

aux_table=[act_coeffs lower' upper' average'];
