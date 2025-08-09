function residual = dynamic_resid(T, y, x, params, steady_state, it_, T_flag)
% function residual = dynamic_resid(T, y, x, params, steady_state, it_, T_flag)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T             [#temp variables by 1]     double   vector of temporary terms to be filled by function
%   y             [#dynamic variables by 1]  double   vector of endogenous variables in the order stored
%                                                     in M_.lead_lag_incidence; see the Manual
%   x             [nperiods by M_.exo_nbr]   double   matrix of exogenous variables (in declaration order)
%                                                     for all simulation periods
%   steady_state  [M_.endo_nbr by 1]         double   vector of steady state values
%   params        [M_.param_nbr by 1]        double   vector of parameter values in declaration order
%   it_           scalar                     double   time period for exogenous variables for which
%                                                     to evaluate the model
%   T_flag        boolean                    boolean  flag saying whether or not to calculate temporary terms
%
% Output:
%   residual
%

if T_flag
    T = orthodox.dynamic_resid_tt(T, y, x, params, steady_state, it_);
end
residual = zeros(32, 1);
    residual(1) = (y(45)) - (y(61));
    residual(2) = (y(46)) - (y(66));
    residual(3) = (y(27)) - (y(45)+T(2)*(T(1)/(1+T(1))*y(7)+1/(1+T(1))*y(63)+T(3)*(y(35)-y(64))+y(48)-y(33)));
    residual(4) = (y(28)) - (T(4)*(y(2)+params(6)*params(14)*y(59)+1/(params(6)*params(6)*params(26))*y(29))+y(49));
    residual(5) = (y(29)) - (T(5)*y(65)-y(66)+T(6)*y(60));
    residual(6) = (y(30)) - ((1-params(17))*y(4)+y(28)*params(17)+y(49)*params(26)*params(6)*params(6)*params(17));
    residual(7) = (y(31)) - ((y(27)-params(37)*y(1)-params(38)*(1-params(37))*y(34)-params(39)*(y(34)-y(8))-y(50))/((1-params(37))*params(36)));
    residual(8) = (y(43)) - (T(8)*(T(4)*y(6)+T(9)*y(62)+params(32)/(1+params(6)*params(14))*y(5)-y(31)*(1+params(6)*params(14)*params(32))/(1+params(6)*params(14))+y(45)*T(9)+T(7)*T(10)+y(53)));
    residual(9) = (y(44)) - (T(10)-(y(31)-y(13))+y(54));
    residual(10) = (y(32)) - (y(43)*params(41)+y(44)*(1-params(41)));
    residual(11) = (y(33)) - ((y(34)-y(28)*params(21)-y(47)-params(23)*T(11)*y(36)-0.01*y(39))/params(22));
    residual(12) = (y(34)) - (params(35)*(params(40)*(y(4)+T(11)*y(36))+y(35)*(1-params(40))+y(51)));
    residual(13) = (y(35)) - (y(4)+T(11)*y(36)+y(36)-y(32));
    residual(14) = (y(36)) - (params(42)*(y(51)+((y(31)-y(52))*(1+params(6)*params(14)*params(33))-(y(45)*params(6)*params(14)+y(5)*params(33)))/T(12)-y(32)*(1-params(40)))/params(40)+(1-params(42))*((1-params(40))/params(40)*(-y(32))+y(51)/params(40)));
    residual(15) = (y(37)) - ((-params(43))*(y(39)-y(29)-y(30))-params(46)*y(41)+y(55));
    residual(16) = (y(38)) - (y(9)+y(1)-y(13));
    residual(17) = (y(39)) - (1.7*y(40)-0.7*y(14)+0.99*y(10)+y(56));
    residual(18) = (y(40)) - (T(5)*(y(34)-y(30))+y(29)*T(6)-y(3));
    residual(19) = (y(41)) - (y(11)+params(45)*(y(42)-y(12))+y(57));
    residual(20) = (y(42)) - (y(30)*1.17278+y(41)*0.073148-y(39)*0.245928);
    residual(21) = (y(47)) - (params(54)*y(15)+params(55)*x(it_, 5)+x(it_, 1));
    residual(22) = (y(48)) - (params(56)*y(16)+x(it_, 2));
    residual(23) = (y(49)) - (params(57)*y(17)+x(it_, 3));
    residual(24) = (y(50)) - (params(58)*y(18)+x(it_, 4));
    residual(25) = (y(51)) - (x(it_, 5)+y(19)+params(59)*(y(19)-y(26)));
    residual(26) = (y(52)) - (params(60)*y(20)+x(it_, 6));
    residual(27) = (y(53)) - (params(61)*y(21)+x(it_, 7));
    residual(28) = (y(54)) - (params(62)*y(22)+x(it_, 8));
    residual(29) = (y(55)) - (params(63)*y(23)+x(it_, 9));
    residual(30) = (y(56)) - (params(64)*y(24)+x(it_, 10));
    residual(31) = (y(57)) - (params(65)*y(25)+x(it_, 11));
    residual(32) = (y(58)) - (y(19));

end
