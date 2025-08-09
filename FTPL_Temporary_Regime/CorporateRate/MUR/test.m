clear
clc
nvars=32;
load resid_inno.mat
load start_lagged_pinf.mat
load IIcoef.mat
load USdata_1959_2017.mat
act_data=data';
nboot=8; 
tic
[boots_data, NKweight_output] = Boots_data_endog_switching1(act_data,nvars,inno,nboot,start_lagged_pinf,coef);
toc

%%
[~, T]=size(act_data);
% YY=zeros(size(A,1),T,nboot);
YY=zeros(nvars,T,nboot);
NKweight_output=NaN(nvars+1,3,nboot);
for i=1:nboot
    disp(i);
    % y = zeros(size(A,1),T+1);
    y = zeros(nvars,T+1);
    % inno=inno(:,sum(isnan(inno),1)==0);     % Delete NaN observations
    inno=inno(:,sum(isnan(inno),1)==0);     % Delete NaN observations
    rng('shuffle');
    inx = randi(size(inno,2),T+1,1);
    %y(1:npar,1)=act_data(:,1);

    lagged_pinf=NaN(T+1+16,1);
    lagged_pinf(1:16,1)=start_lagged_pinf;
    for t=2:T+1
        mean_lagged_pinf=mean(lagged_pinf(1+(t-2):16+(t-2)));
        coef(16)=exp(-coef(21)*abs(mean_lagged_pinf)); % NKwage
        coef(17)=exp(-coef(22)*abs(mean_lagged_pinf)); % NKprice
        oo_=orthodox.driver_function(coef);
        A=oo_.dr.ghx;
        B=oo_.dr.ghu;

        y(oo_.dr.order_var,t) =  A*y(oo_.dr.state_var,t-1)+ B*inno(:,inx(t));
        lagged_pinf(16+(t-1),1)=y(5,t);
        NKweight_output(t,:,i)=[mean_lagged_pinf coef(16) coef(17)];
    end
    YY(:,:,i)=y(:,2:end);
end
toc
