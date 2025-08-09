function [T_order, T] = static_g1_tt(y, x, params, T_order, T)
if T_order >= 1
    return
end
[T_order, T] = orthodox.sparse.static_resid_tt(y, x, params, T_order, T);
T_order = 1;
if size(T, 1) < 12
    T = [T; NaN(12 - size(T, 1), 1)];
end
T(12) = 1/(1-T(1))-T(1)/(1-T(1));
end
