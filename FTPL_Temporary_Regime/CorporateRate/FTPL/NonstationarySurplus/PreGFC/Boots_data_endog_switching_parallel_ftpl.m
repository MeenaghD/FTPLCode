function [YY, NKweight_output, nsims, NB_Tall] = Boots_data_endog_switching_parallel_ftpl_test(act_data, nvars, inno_raw, nboot, start_lagged_pinf, coef, rho_est, NBbase_raw)

% Clean input data
%%
inno_clean = inno_raw(:, sum(isnan(inno_raw), 1) == 0);
NBbase_exp = exp(NBbase_raw ./ 100);

[~, T] = size(act_data);

% Initialize output containers
YY_cell = cell(1, nboot);
NKweight_cell = cell(1, nboot);
NB_Tall_cell = cell(1, nboot);
%%
parfor i = 1:nboot
    % Initialize local variables
    y = zeros(nvars, T);
    yforward = zeros(nvars, T + 49);
    lagged_pinf = NaN(T + 17, 1);
    lagged_pinf(1:16) = start_lagged_pinf;

    stream = RandStream('CombRecursive', 'Seed', 1 + i);
    inx = randi(stream, size(inno_clean, 2), T + 1, 1);

    coef_local = coef;
    rho_est_local = rho_est;
    NBbase_local = NBbase_exp;
    nk_temp = NaN(T, 3);
    NB = zeros(1, T + 49);
    surplus = zeros(1, T + 49);
    NB_T_local = NaN(T - 2, 1);

    surplus(1:2) = act_data(32, 1:2);
    NB(1:2) = exp(act_data(33, 1:2) ./ 100);

    for t = 3:T
        mean_lagged_pinf = mean(lagged_pinf(1 + (t - 3):16 + (t - 3)).^2);

        try
            coef_local(16) = exp(-coef_local(21) * abs(mean_lagged_pinf));
            coef_local(17) = exp(-coef_local(22) * abs(mean_lagged_pinf));
        catch
            coef_local(16) = 0.7;
            coef_local(17) = 0.7;
        end

        [~, oo_] = driver_function(coef_local, rho_est_local);
        A = oo_.dr.ghx;
        B = oo_.dr.ghu;

        r5 = zeros(1, T + 49);
        pinf = zeros(1, T + 49);

        if t == 3
            r5(:) = act_data(1, t);
            pinf(:) = act_data(5, t);
        else
            for t1 = t:T + 49
                r5(t1) = mean(yforward(1, t1:min(t1 + 20, T + 49)));
            end
            pinf(t:T + 49) = yforward(5, t:T + 49);
        end

        for t1 = t:T + 49
            if t1 == t
                surplus(t1) = surplus(t1 - 1) + rho_est_local(13) * (surplus(t1 - 1) - surplus(t1 - 2)) + inno_clean(12, inx(t));
            else
                surplus(t1) = surplus(t1 - 1) + rho_est_local(13) * (surplus(t1 - 1) - surplus(t1 - 2));
            end

            NB(t1) = NB(t1 - 1) - surplus(t1) + ...
                (r5(t1 - 1) / 100) * (NB(t1 - 1) - 20 * ((r5(t1) / 100) - (r5(t1 - 1) / 100)) * NB(t1 - 1)) - ...
                (0.075 * (pinf(t - 1) / 100) * NB(t1));
        end

        if abs(NB(t + 49)) > 1e7
            y = zeros(nvars, T);  % discard the simulation
            break;
        else
            P_T = max(0, 0.02 * NB(t + 49) / (coef(26) * NBbase_local(t + 49)));
        end

        NB_T_local(t - 2) = NB(t + 49);

        % Inject updated pinfbar into a local copy of inno
        inno_mod = inno_clean;
        inno_mod(13, :) = P_T / 49;

        y(oo_.dr.order_var, t) = A * y(oo_.dr.state_var, t - 1) + B * inno_mod(:, inx(t));
        yforward(:, t) = y(:, t);

        for t1 = t + 1:T + 70
            yforward(oo_.dr.order_var, t1) = A * yforward(oo_.dr.state_var, t1 - 1);
        end

        lagged_pinf(16 + (t - 2)) = y(5, t);
        nk_temp(t, :) = [mean_lagged_pinf, coef_local(16), coef_local(17)];
    end

    % Store results in cell arrays
    YY_cell{i} = y(:, 3:end);
    NKweight_cell{i} = nk_temp(3:end, :);
    NB_Tall_cell{i} = NB_T_local;
end
%%
% Filter out failed simulations
valid = cellfun(@(yy) max(yy(:)) ~= 0, YY_cell);

YY = cat(3, YY_cell{valid});
NKweight_output = cat(3, NKweight_cell{valid});
NB_Tall = horzcat(NB_Tall_cell{valid});
nsims = sum(valid);

end