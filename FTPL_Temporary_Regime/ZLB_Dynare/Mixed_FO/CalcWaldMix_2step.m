function [pvalue_novar, Wald_novar, Trans_Wald_novar, mean_var_sims] = CalcWald(dataAll,dataPre,dataPost,coefPre,coefPost)
try
%%
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% FTPL Model for PreGFC
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
pibarPre=mean(dataPre(:,5));
coefPre(16)=exp(-coefPre(21)*abs(pibarPre));
coefPre(17)=exp(-coefPre(22)*abs(pibarPre));

save IIcoefPre coefPre;
load rho_estPre.mat;
rho_estPre=rho_estPre;
% dynare orthodox.mod noclearall nopreprocessoroutput notime;    
[M_FTPL_Pre, oo_FTPL_Pre] = driver_function_ftpl(coefPre,rho_estPre);
warning off
[M_FTPL_Pre_ZLB, oo_FTPL_Pre_ZLB] = driver_function_ftpl_zlb(coefPre,rho_estPre);

% Expected variables: c inv lab inf pk w rk
inx_expect=[7 2 9 5 3 6 10];        % or inx_expect=[];  The variables you used to generate E_t[y_(t+1)] used by LIML

% inx_eqs=[9 1 2 5 10 12 6 7 13 15 17];    % Select Equations that contains model residuals, in the order that you specify exogeneous variables in .mod
inx_eqs=[11 3 4 7 12 14 8 9 15 17 19];    % Select Equations that contains model residuals, (DAVID: CHECK EQUATIONS IN "+modfile/dynamic_resid.m")

act_dataPre=dataPre';                                       % act_data is a k*T matrix

% Step1:
% load orthodox/Output/orthodox_results.mat
% % [residual,inno,rho_est] = GetRes_Exact(fname,act_data,[]);                         % Exact method

[residualPre,residualPre_dt, innoPre,rho_estPre,nst_inxPre] = GetRes_LIMLPre(M_FTPL_Pre,act_dataPre,inx_expect,inx_eqs);   % LIML
save resid_innoPre residualPre residualPre_dt innoPre
save rho_estPre rho_estPre
[residualPre_ZLB,residualPre_dt_ZLB, innoPre_ZLB,rho_estPre_ZLB,nst_inxPre_ZLB] = GetRes_LIMLPre_ZLB(M_FTPL_Pre_ZLB,act_dataPre,inx_expect,inx_eqs);   % LIML
save resid_innoPre_ZLB residualPre_ZLB residualPre_dt_ZLB innoPre_ZLB
save rho_estPre_ZLB rho_estPre_ZLB

% Surplus is an ARIMA(1,1,0), do this separately
dsurplus=diff(dataPre(:,32))';
[rho_surp, surp_inno] = Vare(dsurplus,1,[]);   
rho_estPre(13)=rho_surp;
innoPre(12,:)=[NaN surp_inno];
save resid_innoPre residualPre residualPre_dt innoPre
save rho_estPre rho_estPre
rho_estPre_ZLB(13)=rho_surp;
innoPre_ZLB(12,:)=[NaN surp_inno];
save resid_innoPre_ZLB residualPre_ZLB residualPre_dt_ZLB innoPre_ZLB
save rho_estPre_ZLB rho_estPre_ZLB
warning off
% dynare orthodox.mod noclearall nopreprocessoroutput notime;    
% [M_Pre, oo_Pre] = driver_function_ftpl(coefPre,rho_estPre);
[M_FTPL_Pre, oo_FTPL_Pre] = driver_function_ftpl(coefPre,rho_estPre);
[M_FTPL_Pre_ZLB, oo_FTPL_Pre_ZLB] = driver_function_ftpl_zlb(coefPre,rho_estPre);
nvarsPre=size(oo_FTPL_Pre.dr.ghx,1);
warning on

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
%%
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% Orthodox Model for PostGFC
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

% initiate NK weights using average inflation data
pibarPost=mean(dataPost(:,5).^2);
coefPost(16)=exp(-coefPost(21)*abs(pibarPost));
coefPost(17)=exp(-coefPost(22)*abs(pibarPost));

save IIcoefPost coefPost;
load rho_estPost.mat;
rho_estPost=rho_estPost;
% dynare orthodox.mod noclearall noPostprocessoroutput notime;    
[M_Orthodox_Post, oo_Orthodox_Post] = driver_function_orthodox(coefPost,rho_estPost);
warning off
[M_Orthodox_Post_ZLB, oo_Orthodox_Post_ZLB] = driver_function_orthodox_zlb(coefPost,rho_estPost);
% Expected variables: c inv lab inf pk w rk
inx_expect=[7 2 9 5 3 6 10];        % or inx_expect=[];  The variables you used to generate E_t[y_(t+1)] used by LIML

% inx_eqs=[9 1 2 5 10 12 6 7 13 15 17];    % Select Equations that contains model residuals, in the order that you specify exogeneous variables in .mod
inx_eqs=[11 3 4 7 12 14 8 9 15 17 19];    % Select Equations that contains model residuals, (DAVID: CHECK EQUATIONS IN "+modfile/dynamic_resid.m")

act_dataPost=dataPost';                                       % act_data is a k*T matrix

% Step1:
% load orthodox/Output/orthodox_results.mat
% % [residual,inno,rho_est] = GetRes_Exact(fname,act_data,[]);                         % Exact method

[residualPost,residualPost_dt, innoPost,rho_estPost,nst_inxPost] = GetRes_LIMLPost(M_Orthodox_Post,act_dataPost,inx_expect,inx_eqs);   % LIML
save resid_innoPost residualPost residualPost_dt innoPost
save rho_estPost rho_estPost
[residualPost_ZLB,residualPost_dt_ZLB, innoPost_ZLB,rho_estPost_ZLB,nst_inxPost_ZLB] = GetRes_LIMLPost_ZLB(M_Orthodox_Post_ZLB,act_dataPost,inx_expect,inx_eqs);   % LIML
save resid_innoPost_ZLB residualPost_ZLB residualPost_dt_ZLB innoPost_ZLB
save rho_estPost_ZLB rho_estPost_ZLB

% Surplus is an ARIMA(1,1,0), do this separately
dsurplus=diff(dataPost(:,32))';
[rho_surp, surp_inno] = Vare(dsurplus,1,[]);   
rho_estPost(13)=rho_surp;
innoPost(12,:)=[NaN surp_inno];
save resid_innoPost residualPost residualPost_dt innoPost
save rho_estPost rho_estPost
rho_estPost_ZLB(13)=rho_surp;
innoPost_ZLB(12,:)=[NaN surp_inno];
save resid_innoPost_ZLB residualPost_ZLB residualPost_dt_ZLB innoPost_ZLB
save rho_estPost_ZLB rho_estPost_ZLB

warning off
% dynare orthodox.mod noclearall nopreprocessoroutput notime;    
[M_Orthodox_Post, oo_Orthodox_Post] = driver_function_orthodox(coefPost,rho_estPost);
[M_Orthodox_Post_ZLB, oo_Orthodox_Post_ZLB] = driver_function_orthodox_zlb(coefPost,rho_estPost);
nvarsPost=size(oo_Orthodox_Post.dr.ghx,1);
warning on

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 

%%
% Step2:
% Boostrap data by function Boots_data();

load start_lagged_pinf.mat
nboot=500; 
load NBbase.mat

% Running the code in serial was slow because it takes 20 seconds per simulation
% [boots_data, NKweight_output] = Boots_data_endog_switching(act_data,nvars,inno,nboot,start_lagged_pinf,coef);

% Running the code in parallel can use 8 cores. 
% You can't load the coefficients within the parfor loop.
% To do this we need to change the FILENAME.driver into a function that has the coef and rho_est as input that returns oo_.
% Also, I commented out the code that save the results and writes error messages at the end of the file. 
% Save this file as driver_function in the current directory, that is used in Boots_data_endog_switching_parallel
%%
warning off
% load surplus.mat
disp('Bootstrap Pre start')
[boots_dataPre, NKweight_output, nsims, NB_Tall] = Boots_data_endog_switching_parallel_ftpl_ZLB(act_dataPre,nvarsPre,innoPre,innoPre_ZLB,nboot,start_lagged_pinf,coefPre,rho_estPre,rho_estPre_ZLB,NBbase);
disp('Bootstrap Post start')

% [boots_dataPre, NKweight_output] = Boots_data_endog_switching_parallel_orthodox_ZLB(act_dataPre,nvarsPre,innoPre,innoPre_ZLB,nboot,start_lagged_pinf,coefPre,rho_estPre,rho_estPre_ZLB);
[boots_data, NKweight_output] = Boots_data_endog_switching_parallel_orthodox_ZLB(act_dataPost,boots_dataPre,nvarsPost,innoPost,innoPost_ZLB,nboot,start_lagged_pinf,coefPost,rho_estPost,rho_estPost_ZLB);
disp('Bootstrap finished')
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
boots_inx=M_Orthodox_Post.orig_endo_nbr - M_Orthodox_Post.exo_nbr + nst_inx;
boots_nonstatResid = boots_data(boots_inx,:,:);
act_dataAll=dataAll';
disp('Wald start')
 
[pvalue_novar, Wald_novar, Trans_Wald_novar, all_coeffs_novar, act_coeffs_novar, mean_var_sims_novar] = Wald_nonstationary(act_dataAll(var_no,3:end),boots_data(var_no,:,:),act_nonstatResid(1,3:end),boots_nonstatResid,rho_nonstat,var_order,0);
% delete([fname '.log'],[fname '.m'],[fname '_static.m'],[fname '_set_auxiliary_variables.m']);
catch
    pvalue_novar=0;
    Wald_novar=1000;
    Trans_Wald_novar=1000;
    mean_var_sims=[1000 1000 1000];
end

end