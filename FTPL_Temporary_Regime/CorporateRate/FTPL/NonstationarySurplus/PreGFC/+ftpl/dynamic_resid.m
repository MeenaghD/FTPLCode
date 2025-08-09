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
    T = ftpl.dynamic_resid_tt(T, y, x, params, steady_state, it_);
end
residual = zeros(34, 1);
    residual(1) = (y(46)) - (x(it_, 13));
    residual(2) = (y(47)) - (y(68));
    residual(3) = (y(28)) - (y(46)+T(2)*(T(1)/(1+T(1))*y(7)+1/(1+T(1))*y(65)+T(3)*(y(36)-y(66))+y(49)-y(34)));
    residual(4) = (y(29)) - (T(4)*(y(2)+params(6)*params(14)*y(62)+1/(params(6)*params(6)*params(26))*y(30))+y(50));
    residual(5) = (y(30)) - (T(5)*y(67)-y(68)+T(6)*y(63));
    residual(6) = (y(31)) - ((1-params(17))*y(4)+y(29)*params(17)+y(50)*params(26)*params(6)*params(6)*params(17));
    residual(7) = (y(32)) - ((y(28)-params(37)*y(1)-params(38)*(1-params(37))*y(35)-params(39)*(y(35)-y(8))-y(51))/((1-params(37))*params(36)));
    residual(8) = (y(44)) - (T(8)*(T(4)*y(6)+T(9)*y(64)+params(32)/(1+params(6)*params(14))*y(5)-y(32)*(1+params(6)*params(14)*params(32))/(1+params(6)*params(14))+y(46)*T(9)+T(7)*T(10)+y(54)));
    residual(9) = (y(45)) - (T(10)-(y(32)-y(46))+y(55));
    residual(10) = (y(33)) - (y(44)*params(41)+y(45)*(1-params(41)));
    residual(11) = (y(34)) - ((y(35)-y(29)*params(21)-y(48)-params(23)*T(11)*y(37)-0.01*y(40))/params(22));
    residual(12) = (y(35)) - (params(35)*(params(40)*(y(4)+T(11)*y(37))+y(36)*(1-params(40))+y(52)));
    residual(13) = (y(36)) - (y(4)+T(11)*y(37)+y(37)-y(33));
    residual(14) = (y(37)) - (params(42)*(y(52)+((y(32)-y(53))*(1+params(6)*params(14)*params(33))-(y(46)*params(6)*params(14)+y(5)*params(33)))/T(12)-y(33)*(1-params(40)))/params(40)+(1-params(42))*((1-params(40))/params(40)*(-y(33))+y(52)/params(40)));
    residual(15) = (y(38)) - ((-params(43))*(y(40)-y(30)-y(31))-params(46)*y(42)+y(56));
    residual(16) = (y(39)) - (y(9)+y(1)-y(46));
    residual(17) = (y(40)) - (1.7*y(41)-0.7*y(13)+0.99*y(10)+y(57));
    residual(18) = (y(41)) - (T(5)*(y(35)-y(31))+y(30)*T(6)-y(3));
    residual(19) = (y(42)) - (y(11)+params(45)*(y(43)-y(12))+y(58));
    residual(20) = (y(43)) - (y(31)*1.17278+y(42)*0.073148-y(40)*0.245928);
    residual(21) = (y(48)) - (params(54)*y(14)+params(55)*x(it_, 5)+x(it_, 1));
    residual(22) = (y(49)) - (params(56)*y(15)+x(it_, 2));
    residual(23) = (y(50)) - (params(57)*y(16)+x(it_, 3));
    residual(24) = (y(51)) - (params(58)*y(17)+x(it_, 4));
    residual(25) = (y(52)) - (x(it_, 5)+y(18)+params(59)*(y(18)-y(26)));
    residual(26) = (y(53)) - (params(60)*y(19)+x(it_, 6));
    residual(27) = (y(54)) - (params(61)*y(20)+x(it_, 7));
    residual(28) = (y(55)) - (params(62)*y(21)+x(it_, 8));
    residual(29) = (y(56)) - (params(63)*y(22)+x(it_, 9));
    residual(30) = (y(57)) - (params(64)*y(23)+x(it_, 10));
    residual(31) = (y(58)) - (params(65)*y(24)+x(it_, 11));
    residual(32) = (y(59)) - (x(it_, 12)+y(25)+params(66)*(y(25)-y(27)));
    residual(33) = (y(60)) - (y(18));
    residual(34) = (y(61)) - (y(25));

end
