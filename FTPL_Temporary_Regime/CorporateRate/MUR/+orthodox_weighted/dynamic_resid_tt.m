function T = dynamic_resid_tt(T, y, x, params, steady_state, it_)
% function T = dynamic_resid_tt(T, y, x, params, steady_state, it_)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T             [#temp variables by 1]     double  vector of temporary terms to be filled by function
%   y             [#dynamic variables by 1]  double  vector of endogenous variables in the order stored
%                                                    in M_.lead_lag_incidence; see the Manual
%   x             [nperiods by M_.exo_nbr]   double  matrix of exogenous variables (in declaration order)
%                                                    for all simulation periods
%   steady_state  [M_.endo_nbr by 1]         double  vector of steady state values
%   params        [M_.param_nbr by 1]        double  vector of parameter values in declaration order
%   it_           scalar                     double  time period for exogenous variables for which
%                                                    to evaluate the model
%
% Output:
%   T           [#temp variables by 1]       double  vector of temporary terms
%

assert(length(T) >= 12);

T(1) = params(28)/params(6);
T(2) = 1/((1-T(1))/(params(27)*(1+T(1))));
T(3) = (params(27)-1)*params(24)/(params(27)*(1+T(1)));
T(4) = 1/(1+params(6)*params(14));
T(5) = params(15)/(params(15)+1-params(7));
T(6) = (1-params(7))/(params(15)+1-params(7));
T(7) = (1-params(29))*(1-params(6)*params(14)*params(29))/((1+params(6)*params(14))*params(29))*1/(1+(params(8)-1)*params(11));
T(8) = 1/(1+T(7));
T(9) = params(6)*params(14)/(1+params(6)*params(14));
T(10) = y(36)*params(30)+y(34)*1/(1-T(1))-y(7)*T(1)/(1-T(1));
T(11) = 1/(params(34)/(1-params(34)));
T(12) = (1-params(31))*(1-params(6)*params(14)*params(31))/params(31)/(1+(params(35)-1)*params(10));

end
