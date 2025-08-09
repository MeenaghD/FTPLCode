function [ YY, NKweight_output ] = Boots_data_endog_switching_parallel_orthodox_ZLB_NB(act_data,simsPre,nvars,inno,inno_ZLB,nboot,start_lagged_pinf,coef,rho_est,rho_est_ZLB)
%Input:
%	nboot:  number of bootstraps.
%	A:  oo_.dr.ghx
%	B:  oo_.dr.ghx
%   inno: model innovation
%Output:
%	YY:   k*T*nboot matrix
% load([fname '_results.mat']);
[~, TPre,~] = size(simsPre);
[~, TPost] = size(act_data);
T=TPre+TPost;

YY = zeros(nvars, T+1, nboot);
NKweight_output = NaN(T+1, 3, nboot);  % note: nvars+1 was incorrect based on usage

inno = inno(:, sum(isnan(inno), 1) == 0);  % Remove NaNs
inno_ZLB = inno_ZLB(:, sum(isnan(inno_ZLB), 1) == 0);  % Remove NaNs

for i = 1:nboot
    y = zeros(nvars, T + 1);
    y(:,1:TPre)=simsPre(:,:,i);
    yforward = zeros(nvars, T + 49);
    coef_local = coef;  % local copy for thread safety
    rho_est_local = rho_est;  % local copy for thread safety
    rho_est_local_ZLB = rho_est_ZLB;  % local copy for thread safety
% % Setting the simulations to start at the data, not 0. 
% % Need to add 32nd variable which is A(-2).
%     act_data_local=act_data;
%     [M_, oo_] = driver_function(coef_local,rho_est_local);
%     act_data_local(32,:)=zeros(1,size(act_data_local,2));
%     y(:,1)=act_data_local(:,1);


    lagged_pinf = NaN(T + 1 + 16, 1);
    % lagged_pinf(1:16, 1) = start_lagged_pinf;
    lagged_pinf(1:TPre, 1) = simsPre(5,:,i)';
% Create a reproducible random stream unique to this iteration
    stream = RandStream('CombRecursive','Seed',1+i); 
    inx = randi(stream, size(inno,2), T+1, 1);
    % inx = randi(size(inno, 2), T + 1, 1);

    for t = TPre+1:T + 1
        % mean_lagged_pinf = mean(lagged_pinf(1 + (t - 3):16 + (t - 3)).^2);
        mean_lagged_pinf = mean(lagged_pinf(t-17:t-1).^2);
        coef_local(16) = exp(-coef_local(21) * abs(mean_lagged_pinf));
        coef_local(17) = exp(-coef_local(22) * abs(mean_lagged_pinf));

        % oo_ = orthodox.driver_function(coef_local);
        % [M_, oo_] = driver_function(coef_local,rho_est_local);
        [M_, oo_] = driver_function_orthodox(coef_local,rho_est_local);
        A=oo_.dr.ghx;
        B=oo_.dr.ghu;

        y(oo_.dr.order_var, t) = A * y(oo_.dr.state_var, t - 1) + B * inno(:, inx(t));
        lagged_pinf(t, 1) = y(5, t);


        % ZLB cutoff from calc_ZLBcutoff.m
        if y(1,t)<-1.275
            y(1,t-1)=-1.275;
            [M_ZLB, oo_ZLB] = driver_function_orthodox_zlb(coef_local,rho_est_local_ZLB);
            A_ZLB=oo_ZLB.dr.ghx;
            B_ZLB=oo_ZLB.dr.ghu;

            y(oo_ZLB.dr.order_var, t) = A_ZLB * y(oo_ZLB.dr.state_var, t - 1) + B_ZLB * inno_ZLB(:, inx(t));
            % y(1,t)=-4.4230;
            % lagged_pinf(16 + (t - 1), 1) = y(5, t);
            lagged_pinf(t, 1) = y(5, t);
            nk_temp(t, :) = [mean_lagged_pinf coef_local(16) coef_local(17)];
        end            
    end

    YY(:, :, i) = y(:, :);
    NKweight_output(:, :, i) = nk_temp(:,:);  % <- Assign after the loop
end
