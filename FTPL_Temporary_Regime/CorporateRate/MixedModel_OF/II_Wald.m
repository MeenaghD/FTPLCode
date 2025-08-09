clear
clc

load est_coef.mat
dataAll=load('USdata_1959_2017.mat');
dataPre=load('USdata_1959_2007.mat');
dataPost=load('USdata_2008_2017.mat');
[value_novar, Wald_novar, Trans_Wald_novar, mean_var_sims] = CalcWaldMix(dataAll.data,dataPre.data,dataPost.data,coefPre,coefPost);
