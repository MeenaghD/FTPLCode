clear
clc

load est_coef.mat
load USdata_1959_2006.mat
[nsims, pvalue_novar, Wald_novar, Trans_Wald_novar, mean_var_sims] = CalcWald(data,coef);
