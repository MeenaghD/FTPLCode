function T = static_resid_tt(T, y, x, params)
% function T = static_resid_tt(T, y, x, params)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T         [#temp variables by 1]  double   vector of temporary terms to be filled by function
%   y         [M_.endo_nbr by 1]      double   vector of endogenous variables in declaration order
%   x         [M_.exo_nbr by 1]       double   vector of exogenous variables in declaration order
%   params    [M_.param_nbr by 1]     double   vector of parameter values in declaration order
%
% Output:
%   T         [#temp variables by 1]  double   vector of temporary terms
%

assert(length(T) >= 11);

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
