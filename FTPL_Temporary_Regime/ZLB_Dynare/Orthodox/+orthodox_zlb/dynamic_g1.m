function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
% function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
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
%   g1
%

if T_flag
    T = orthodox_zlb.dynamic_g1_tt(T, y, x, params, steady_state, it_);
end
g1 = zeros(32, 74);
g1(1,60)=(-1);
g1(1,44)=1;
g1(2,63)=(-1);
g1(2,45)=1;
g1(3,1)=(-1);
g1(3,26)=1;
g1(4,2)=(-T(1));
g1(4,27)=1;
g1(4,58)=(-(params(14)*params(6)*T(1)));
g1(4,28)=(-(T(1)*1/(params(6)*params(6)*params(26))));
g1(4,48)=(-1);
g1(5,28)=1;
g1(5,59)=(-T(3));
g1(5,62)=(-T(2));
g1(5,63)=1;
g1(6,27)=(-params(17));
g1(6,4)=(-(1-params(17)));
g1(6,29)=1;
g1(6,48)=(-(params(26)*params(6)*params(6)*params(17)));
g1(7,1)=(-((-params(37))/((1-params(37))*params(36))));
g1(7,26)=(-(1/((1-params(37))*params(36))));
g1(7,30)=1;
g1(7,8)=(-(params(39)/((1-params(37))*params(36))));
g1(7,33)=(-(((-(params(38)*(1-params(37))))-params(39))/((1-params(37))*params(36))));
g1(7,49)=(-((-1)/((1-params(37))*params(36))));
g1(8,5)=(-(T(5)*params(32)/(1+params(14)*params(6))));
g1(8,30)=(-(T(5)*(-((1+params(14)*params(6)*params(32))/(1+params(14)*params(6))))));
g1(8,6)=(-(T(1)*T(5)));
g1(8,61)=(-(T(5)*T(6)));
g1(8,7)=(-(T(5)*T(4)*(-T(8))));
g1(8,32)=(-(T(5)*T(4)*T(7)));
g1(8,34)=(-(T(5)*T(4)*params(30)));
g1(8,42)=1;
g1(8,44)=(-(T(5)*T(6)));
g1(8,52)=(-T(5));
g1(9,30)=1;
g1(9,7)=T(8);
g1(9,32)=(-T(7));
g1(9,34)=(-params(30));
g1(9,43)=1;
g1(9,12)=(-1);
g1(9,53)=(-1);
g1(10,31)=1;
g1(10,42)=(-params(41));
g1(10,43)=(-(1-params(41)));
g1(11,27)=(-((-params(21))/params(22)));
g1(11,32)=1;
g1(11,33)=(-(1/params(22)));
g1(11,35)=(-((-(params(23)*T(10)))/params(22)));
g1(11,38)=(-((-0.01)/params(22)));
g1(11,46)=(-((-1)/params(22)));
g1(12,4)=(-(params(35)*params(40)));
g1(12,33)=1;
g1(12,34)=(-(params(35)*(1-params(40))));
g1(12,35)=(-(params(35)*T(10)*params(40)));
g1(12,50)=(-params(35));
g1(13,4)=(-1);
g1(13,31)=1;
g1(13,34)=1;
g1(13,35)=(-(1+T(10)));
g1(14,5)=(-(params(42)*(-params(33))/T(11)/params(40)));
g1(14,30)=(-(params(42)*(1+params(14)*params(6)*params(33))/T(11)/params(40)));
g1(14,31)=(-(params(42)*(-(1-params(40)))/params(40)+(1-params(42))*(-((1-params(40))/params(40)))));
g1(14,35)=1;
g1(14,44)=(-(params(42)*(-(params(14)*params(6)))/T(11)/params(40)));
g1(14,50)=(-(params(42)*1/params(40)+(1-params(42))*1/params(40)));
g1(14,51)=(-(params(42)*(-(1+params(14)*params(6)*params(33)))/T(11)/params(40)));
g1(15,28)=(-params(43));
g1(15,29)=(-params(43));
g1(15,36)=1;
g1(15,38)=params(43);
g1(15,40)=params(46);
g1(15,54)=(-1);
g1(16,1)=(-1);
g1(16,9)=(-1);
g1(16,37)=1;
g1(16,12)=1;
g1(17,10)=(-0.99);
g1(17,38)=1;
g1(17,39)=(-1.7);
g1(17,13)=0.7;
g1(17,55)=(-1);
g1(18,3)=1;
g1(18,28)=(-T(3));
g1(18,29)=T(2);
g1(18,33)=(-T(2));
g1(18,39)=1;
g1(19,36)=(-params(44));
g1(19,11)=(-1);
g1(19,40)=1;
g1(19,56)=(-1);
g1(20,29)=(-1.17278);
g1(20,38)=0.245928;
g1(20,40)=(-0.073148);
g1(20,41)=1;
g1(21,14)=(-params(54));
g1(21,46)=1;
g1(21,64)=(-1);
g1(21,68)=(-params(55));
g1(22,15)=(-params(56));
g1(22,47)=1;
g1(22,65)=(-1);
g1(23,16)=(-params(57));
g1(23,48)=1;
g1(23,66)=(-1);
g1(24,17)=(-params(58));
g1(24,49)=1;
g1(24,67)=(-1);
g1(25,18)=(-(1+params(59)));
g1(25,50)=1;
g1(25,68)=(-1);
g1(25,25)=params(59);
g1(26,19)=(-params(60));
g1(26,51)=1;
g1(26,69)=(-1);
g1(27,20)=(-params(61));
g1(27,52)=1;
g1(27,70)=(-1);
g1(28,21)=(-params(62));
g1(28,53)=1;
g1(28,71)=(-1);
g1(29,22)=(-params(63));
g1(29,54)=1;
g1(29,72)=(-1);
g1(30,23)=(-params(64));
g1(30,55)=1;
g1(30,73)=(-1);
g1(31,24)=(-params(65));
g1(31,56)=1;
g1(31,74)=(-1);
g1(32,18)=(-1);
g1(32,57)=1;

end
