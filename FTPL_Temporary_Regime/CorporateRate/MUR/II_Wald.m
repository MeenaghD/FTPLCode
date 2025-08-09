clear
clc

load est_coef.mat
load USdata_1959_2017.mat
[nsims, pvalue_novar, Wald_novar, Trans_Wald_novar, mean_var_sims, pcFTPLweight1, mean_FTPLweight] = CalcWald(data,coef);
