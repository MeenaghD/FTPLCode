function [T_order, T] = static_resid_tt(y, x, params, T_order, T)
if T_order >= 0
    return
end
T_order = 0;
if size(T, 1) < 11
    T = [T; NaN(11 - size(T, 1), 1)];
end
T(1) = 1/(1+params(14)*params(6));
T(2) = params(15)/(params(15)+1-params(7));
T(3) = (1-params(7))/(params(15)+1-params(7));
T(4) = (1-params(29))*(1-params(14)*params(6)*params(29))/((1+params(14)*params(6))*params(29))*1/(1+(params(8)-1)*params(11));
T(5) = 1/(1+T(4));
T(6) = params(14)*params(6)/(1+params(14)*params(6));
T(7) = 1/(1-params(28)/params(6));
T(8) = params(28)/params(6)/(1-params(28)/params(6));
T(9) = params(30)*y(9)+T(7)*y(7)-y(7)*T(8);
T(10) = 1/(params(34)/(1-params(34)));
T(11) = (1-params(31))*(1-params(14)*params(6)*params(31))/params(31)/(1+(params(35)-1)*params(10));
end
