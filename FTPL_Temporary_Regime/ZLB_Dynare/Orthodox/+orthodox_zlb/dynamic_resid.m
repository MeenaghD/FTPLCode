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
    T = orthodox_zlb.dynamic_resid_tt(T, y, x, params, steady_state, it_);
end
residual = zeros(32, 1);
    residual(1) = (y(44)) - (y(60));
    residual(2) = (y(45)) - (y(63));
    residual(3) = (y(26)) - (y(1));
    residual(4) = (y(27)) - (T(1)*(y(2)+params(14)*params(6)*y(58)+1/(params(6)*params(6)*params(26))*y(28))+y(48));
    residual(5) = (y(28)) - (T(2)*y(62)-y(63)+T(3)*y(59));
    residual(6) = (y(29)) - ((1-params(17))*y(4)+y(27)*params(17)+y(48)*params(26)*params(6)*params(6)*params(17));
    residual(7) = (y(30)) - ((y(26)-y(1)*params(37)-params(38)*(1-params(37))*y(33)-params(39)*(y(33)-y(8))-y(49))/((1-params(37))*params(36)));
    residual(8) = (y(42)) - (T(5)*(T(1)*y(6)+T(6)*y(61)+params(32)/(1+params(14)*params(6))*y(5)-y(30)*(1+params(14)*params(6)*params(32))/(1+params(14)*params(6))+y(44)*T(6)+T(4)*T(9)+y(52)));
    residual(9) = (y(43)) - (T(9)-(y(30)-y(12))+y(53));
    residual(10) = (y(31)) - (y(42)*params(41)+y(43)*(1-params(41)));
    residual(11) = (y(32)) - ((y(33)-y(27)*params(21)-y(46)-params(23)*T(10)*y(35)-0.01*y(38))/params(22));
    residual(12) = (y(33)) - (params(35)*(params(40)*(y(4)+T(10)*y(35))+y(34)*(1-params(40))+y(50)));
    residual(13) = (y(34)) - (y(4)+T(10)*y(35)+y(35)-y(31));
    residual(14) = (y(35)) - (params(42)*(y(50)+((y(30)-y(51))*(1+params(14)*params(6)*params(33))-(y(44)*params(14)*params(6)+y(5)*params(33)))/T(11)-y(31)*(1-params(40)))/params(40)+(1-params(42))*((1-params(40))/params(40)*(-y(31))+y(50)/params(40)));
    residual(15) = (y(36)) - ((-params(43))*(y(38)-y(28)-y(29))-params(46)*y(40)+y(54));
    residual(16) = (y(37)) - (y(9)+y(1)-y(12));
    residual(17) = (y(38)) - (1.7*y(39)-0.7*y(13)+0.99*y(10)+y(55));
    residual(18) = (y(39)) - (T(2)*(y(33)-y(29))+y(28)*T(3)-y(3));
    residual(19) = (y(40)) - (y(11)+y(36)*params(44)+y(56));
    residual(20) = (y(41)) - (y(29)*1.17278+y(40)*0.073148-y(38)*0.245928);
    residual(21) = (y(46)) - (params(54)*y(14)+params(55)*x(it_, 5)+x(it_, 1));
    residual(22) = (y(47)) - (params(56)*y(15)+x(it_, 2));
    residual(23) = (y(48)) - (params(57)*y(16)+x(it_, 3));
    residual(24) = (y(49)) - (params(58)*y(17)+x(it_, 4));
    residual(25) = (y(50)) - (x(it_, 5)+y(18)+params(59)*(y(18)-y(25)));
    residual(26) = (y(51)) - (params(60)*y(19)+x(it_, 6));
    residual(27) = (y(52)) - (params(61)*y(20)+x(it_, 7));
    residual(28) = (y(53)) - (params(62)*y(21)+x(it_, 8));
    residual(29) = (y(54)) - (params(63)*y(22)+x(it_, 9));
    residual(30) = (y(55)) - (params(64)*y(23)+x(it_, 10));
    residual(31) = (y(56)) - (params(65)*y(24)+x(it_, 11));
    residual(32) = (y(57)) - (y(18));

end
