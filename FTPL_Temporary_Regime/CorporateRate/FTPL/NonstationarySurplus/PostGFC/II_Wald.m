clear
clc

load est_coef.mat
rng('default');

load USdata_2008_2017.mat
[nsims, pvalue_novar, Wald_novar, Trans_Wald_novar, mean_var_sims] = CalcWald(data,coef);
