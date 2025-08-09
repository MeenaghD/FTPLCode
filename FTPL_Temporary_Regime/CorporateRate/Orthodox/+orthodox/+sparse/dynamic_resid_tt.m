function [T_order, T] = dynamic_resid_tt(y, x, params, steady_state, T_order, T)
if T_order >= 0
    return
end
T_order = 0;
if size(T, 1) < 12
    T = [T; NaN(12 - size(T, 1), 1)];
end
T(1) = params(28)/params(6);
T(2) = 1/((1-T(1))/(params(27)*(1+T(1))));
T(3) = (params(27)-1)*params(24)/(params(27)*(1+T(1)));
T(4) = 1/(1+params(6)*params(14));
T(5) = params(15)/(params(15)+1-params(7));
T(6) = (1-params(7))/(params(15)+1-params(7));
T(7) = (1-params(29))*(1-params(6)*params(14)*params(29))/((1+params(6)*params(14))*params(29))*1/(1+(params(8)-1)*params(11));
T(8) = 1/(1+T(7));
T(9) = params(6)*params(14)/(1+params(6)*params(14));
T(10) = y(41)*params(30)+y(39)*1/(1-T(1))-y(7)*T(1)/(1-T(1));
T(11) = 1/(params(34)/(1-params(34)));
T(12) = (1-params(31))*(1-params(6)*params(14)*params(31))/params(31)/(1+(params(35)-1)*params(10));
end
