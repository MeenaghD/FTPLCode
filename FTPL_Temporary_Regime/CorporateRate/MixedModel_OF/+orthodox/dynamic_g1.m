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
    T = orthodox.dynamic_g1_tt(T, y, x, params, steady_state, it_);
end
g1 = zeros(34, 81);
g1(1,65)=(-1);
g1(1,47)=1;
g1(2,70)=(-1);
g1(2,48)=1;
g1(3,29)=1;
g1(3,7)=(-(T(2)*T(1)/(1+T(1))));
g1(3,35)=T(2);
g1(3,67)=(-(T(2)*1/(1+T(1))));
g1(3,37)=(-(T(2)*T(3)));
g1(3,68)=(-(T(2)*(-T(3))));
g1(3,47)=(-1);
g1(3,50)=(-T(2));
g1(4,2)=(-T(4));
g1(4,30)=1;
g1(4,63)=(-(params(6)*params(14)*T(4)));
g1(4,31)=(-(T(4)*1/(params(6)*params(6)*params(26))));
g1(4,51)=(-1);
g1(5,31)=1;
g1(5,64)=(-T(6));
g1(5,69)=(-T(5));
g1(5,70)=1;
g1(6,30)=(-params(17));
g1(6,4)=(-(1-params(17)));
g1(6,32)=1;
g1(6,51)=(-(params(26)*params(6)*params(6)*params(17)));
g1(7,1)=(-((-params(37))/((1-params(37))*params(36))));
g1(7,29)=(-(1/((1-params(37))*params(36))));
g1(7,33)=1;
g1(7,8)=(-(params(39)/((1-params(37))*params(36))));
g1(7,36)=(-(((-(params(38)*(1-params(37))))-params(39))/((1-params(37))*params(36))));
g1(7,52)=(-((-1)/((1-params(37))*params(36))));
g1(8,5)=(-(T(8)*params(32)/(1+params(6)*params(14))));
g1(8,33)=(-(T(8)*(-((1+params(6)*params(14)*params(32))/(1+params(6)*params(14))))));
g1(8,6)=(-(T(4)*T(8)));
g1(8,66)=(-(T(8)*T(9)));
g1(8,7)=(-(T(8)*T(7)*(-(T(1)/(1-T(1))))));
g1(8,35)=(-(T(8)*T(7)*1/(1-T(1))));
g1(8,37)=(-(T(8)*T(7)*params(30)));
g1(8,45)=1;
g1(8,47)=(-(T(8)*T(9)));
g1(8,55)=(-T(8));
g1(9,33)=1;
g1(9,7)=T(1)/(1-T(1));
g1(9,35)=(-(1/(1-T(1))));
g1(9,37)=(-params(30));
g1(9,46)=1;
g1(9,13)=(-1);
g1(9,56)=(-1);
g1(10,34)=1;
g1(10,45)=(-params(41));
g1(10,46)=(-(1-params(41)));
g1(11,30)=(-((-params(21))/params(22)));
g1(11,35)=1;
g1(11,36)=(-(1/params(22)));
g1(11,38)=(-((-(params(23)*T(11)))/params(22)));
g1(11,41)=(-((-0.01)/params(22)));
g1(11,49)=(-((-1)/params(22)));
g1(12,4)=(-(params(35)*params(40)));
g1(12,36)=1;
g1(12,37)=(-(params(35)*(1-params(40))));
g1(12,38)=(-(params(35)*T(11)*params(40)));
g1(12,53)=(-params(35));
g1(13,4)=(-1);
g1(13,34)=1;
g1(13,37)=1;
g1(13,38)=(-(1+T(11)));
g1(14,5)=(-(params(42)*(-params(33))/T(12)/params(40)));
g1(14,33)=(-(params(42)*(1+params(6)*params(14)*params(33))/T(12)/params(40)));
g1(14,34)=(-(params(42)*(-(1-params(40)))/params(40)+(1-params(42))*(-((1-params(40))/params(40)))));
g1(14,38)=1;
g1(14,47)=(-(params(42)*(-(params(6)*params(14)))/T(12)/params(40)));
g1(14,53)=(-(params(42)*1/params(40)+(1-params(42))*1/params(40)));
g1(14,54)=(-(params(42)*(-(1+params(6)*params(14)*params(33)))/T(12)/params(40)));
g1(15,31)=(-params(43));
g1(15,32)=(-params(43));
g1(15,39)=1;
g1(15,41)=params(43);
g1(15,43)=params(46);
g1(15,57)=(-1);
g1(16,1)=(-1);
g1(16,9)=(-1);
g1(16,40)=1;
g1(16,13)=1;
g1(17,10)=(-0.99);
g1(17,41)=1;
g1(17,42)=(-1.7);
g1(17,14)=0.7;
g1(17,58)=(-1);
g1(18,3)=1;
g1(18,31)=(-T(6));
g1(18,32)=T(5);
g1(18,36)=(-T(5));
g1(18,42)=1;
g1(19,11)=(-1);
g1(19,43)=1;
g1(19,12)=params(45);
g1(19,44)=(-params(45));
g1(19,59)=(-1);
g1(20,32)=(-1.17278);
g1(20,41)=0.245928;
g1(20,43)=(-0.073148);
g1(20,44)=1;
g1(21,15)=(-params(54));
g1(21,49)=1;
g1(21,71)=(-1);
g1(21,75)=(-params(55));
g1(22,16)=(-params(56));
g1(22,50)=1;
g1(22,72)=(-1);
g1(23,17)=(-params(57));
g1(23,51)=1;
g1(23,73)=(-1);
g1(24,18)=(-params(58));
g1(24,52)=1;
g1(24,74)=(-1);
g1(25,19)=(-(1+params(59)));
g1(25,53)=1;
g1(25,75)=(-1);
g1(25,27)=params(59);
g1(26,20)=(-params(60));
g1(26,54)=1;
g1(26,76)=(-1);
g1(27,21)=(-params(61));
g1(27,55)=1;
g1(27,77)=(-1);
g1(28,22)=(-params(62));
g1(28,56)=1;
g1(28,78)=(-1);
g1(29,23)=(-params(63));
g1(29,57)=1;
g1(29,79)=(-1);
g1(30,24)=(-params(64));
g1(30,58)=1;
g1(30,80)=(-1);
g1(31,25)=(-params(65));
g1(31,59)=1;
g1(31,81)=(-1);
g1(32,26)=(-(1+params(66)));
g1(32,60)=1;
g1(32,28)=params(66);
g1(33,19)=(-1);
g1(33,61)=1;
g1(34,26)=(-1);
g1(34,62)=1;

end
