function [ YY, NKweight_output] = Boots_data_endog_switching_parallel(act_data,nvars,inno,nboot,start_lagged_pinf,coef,rho_est)
%Input:
%	nboot:  number of bootstraps.
%	A:  oo_.dr.ghx
%	B:  oo_.dr.ghx
%   inno: model innovation
%Output:
%	YY:   k*T*nboot matrix
% load([fname '_results.mat']);
[~, T] = size(act_data);

YY = zeros(nvars, T, nboot);
NKweight_output = NaN(T, 3, nboot);  % note: nvars+1 was incorrect based on usage

inno = inno(:, sum(isnan(inno), 1) == 0);  % Remove NaNs

parfor i = 1:nboot

    y = zeros(nvars, T + 1);
    lagged_pinf = NaN(T + 1 + 16, 1);
    lagged_pinf(1:16, 1) = start_lagged_pinf;
% Create a reproducible random stream unique to this iteration
    stream = RandStream('CombRecursive','Seed',1+i); 
    inx = randi(stream, size(inno,2), T+1, 1);
    % inx = randi(size(inno, 2), T + 1, 1);
    coef_local = coef;  % local copy for thread safety
    rho_est_local = rho_est;  % local copy for thread safety

    nk_temp = NaN(T, 3);  % <- Temporary local variable

    for t = 2:T + 1
        mean_lagged_pinf = mean(lagged_pinf(1 + (t - 2):16 + (t - 2)).^2);
        coef_local(16) = exp(-coef_local(21) * abs(mean_lagged_pinf));
        coef_local(17) = exp(-coef_local(22) * abs(mean_lagged_pinf));

        % oo_ = orthodox.driver_function(coef_local);
        [M_, oo_] = driver_function(coef_local,rho_est_local);
        A=oo_.dr.ghx;
        B=oo_.dr.ghu;

        y(oo_.dr.order_var, t) = A * y(oo_.dr.state_var, t - 1) + B * inno(:, inx(t));
        lagged_pinf(16 + (t - 1), 1) = y(5, t);
        nk_temp(t, :) = [mean_lagged_pinf coef_local(16) coef_local(17)];
    end

    YY(:, :, i) = y(:, 2:end);
    NKweight_output(:, :, i) = nk_temp(2:end,:);  % <- Assign after the loop
end
