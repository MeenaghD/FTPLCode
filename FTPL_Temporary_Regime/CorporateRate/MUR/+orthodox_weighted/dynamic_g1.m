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
    T = orthodox_weighted.dynamic_g1_tt(T, y, x, params, steady_state, it_);
end
g1 = zeros(34, 82);
g1(1,64)=(-(1-params(50)));
g1(1,46)=1;
g1(1,82)=(-params(50));
g1(2,69)=(-1);
g1(2,47)=1;
g1(3,28)=1;
g1(3,7)=(-(T(2)*T(1)/(1+T(1))));
g1(3,34)=T(2);
g1(3,66)=(-(T(2)*1/(1+T(1))));
g1(3,36)=(-(T(2)*T(3)));
g1(3,67)=(-(T(2)*(-T(3))));
g1(3,46)=(-1);
g1(3,49)=(-T(2));
g1(4,2)=(-T(4));
g1(4,29)=1;
g1(4,62)=(-(params(6)*params(14)*T(4)));
g1(4,30)=(-(T(4)*1/(params(6)*params(6)*params(26))));
g1(4,50)=(-1);
g1(5,30)=1;
g1(5,63)=(-T(6));
g1(5,68)=(-T(5));
g1(5,69)=1;
g1(6,29)=(-params(17));
g1(6,4)=(-(1-params(17)));
g1(6,31)=1;
g1(6,50)=(-(params(26)*params(6)*params(6)*params(17)));
g1(7,1)=(-((-params(37))/((1-params(37))*params(36))));
g1(7,28)=(-(1/((1-params(37))*params(36))));
g1(7,32)=1;
g1(7,8)=(-(params(39)/((1-params(37))*params(36))));
g1(7,35)=(-(((-(params(38)*(1-params(37))))-params(39))/((1-params(37))*params(36))));
g1(7,51)=(-((-1)/((1-params(37))*params(36))));
g1(8,5)=(-(T(8)*params(32)/(1+params(6)*params(14))));
g1(8,32)=(-(T(8)*(-((1+params(6)*params(14)*params(32))/(1+params(6)*params(14))))));
g1(8,6)=(-(T(4)*T(8)));
g1(8,65)=(-(T(8)*T(9)));
g1(8,7)=(-(T(8)*T(7)*(-(T(1)/(1-T(1))))));
g1(8,34)=(-(T(8)*T(7)*1/(1-T(1))));
g1(8,36)=(-(T(8)*T(7)*params(30)));
g1(8,44)=1;
g1(8,46)=(-(T(8)*T(9)));
g1(8,54)=(-T(8));
g1(9,32)=1;
g1(9,7)=T(1)/(1-T(1));
g1(9,34)=(-(1/(1-T(1))));
g1(9,36)=(-params(30));
g1(9,45)=1;
g1(9,46)=(-1);
g1(9,55)=(-1);
g1(10,33)=1;
g1(10,44)=(-params(41));
g1(10,45)=(-(1-params(41)));
g1(11,29)=(-((-params(21))/params(22)));
g1(11,34)=1;
g1(11,35)=(-(1/params(22)));
g1(11,37)=(-((-(params(23)*T(11)))/params(22)));
g1(11,40)=(-((-0.01)/params(22)));
g1(11,48)=(-((-1)/params(22)));
g1(12,4)=(-(params(35)*params(40)));
g1(12,35)=1;
g1(12,36)=(-(params(35)*(1-params(40))));
g1(12,37)=(-(params(35)*T(11)*params(40)));
g1(12,52)=(-params(35));
g1(13,4)=(-1);
g1(13,33)=1;
g1(13,36)=1;
g1(13,37)=(-(1+T(11)));
g1(14,5)=(-(params(42)*(-params(33))/T(12)/params(40)));
g1(14,32)=(-(params(42)*(1+params(6)*params(14)*params(33))/T(12)/params(40)));
g1(14,33)=(-(params(42)*(-(1-params(40)))/params(40)+(1-params(42))*(-((1-params(40))/params(40)))));
g1(14,37)=1;
g1(14,46)=(-(params(42)*(-(params(6)*params(14)))/T(12)/params(40)));
g1(14,52)=(-(params(42)*1/params(40)+(1-params(42))*1/params(40)));
g1(14,53)=(-(params(42)*(-(1+params(6)*params(14)*params(33)))/T(12)/params(40)));
g1(15,30)=(-params(43));
g1(15,31)=(-params(43));
g1(15,38)=1;
g1(15,40)=params(43);
g1(15,42)=params(46);
g1(15,56)=(-1);
g1(16,1)=(-1);
g1(16,9)=(-1);
g1(16,39)=1;
g1(16,46)=1;
g1(17,10)=(-0.99);
g1(17,40)=1;
g1(17,41)=(-1.7);
g1(17,13)=0.7;
g1(17,57)=(-1);
g1(18,3)=1;
g1(18,30)=(-T(6));
g1(18,31)=T(5);
g1(18,35)=(-T(5));
g1(18,41)=1;
g1(19,11)=(-1);
g1(19,42)=1;
g1(19,12)=params(45);
g1(19,43)=(-params(45));
g1(19,58)=(-1);
g1(20,31)=(-1.17278);
g1(20,40)=0.245928;
g1(20,42)=(-0.073148);
g1(20,43)=1;
g1(21,14)=(-params(54));
g1(21,48)=1;
g1(21,70)=(-1);
g1(21,74)=(-params(55));
g1(22,15)=(-params(56));
g1(22,49)=1;
g1(22,71)=(-1);
g1(23,16)=(-params(57));
g1(23,50)=1;
g1(23,72)=(-1);
g1(24,17)=(-params(58));
g1(24,51)=1;
g1(24,73)=(-1);
g1(25,18)=(-(1+params(59)));
g1(25,52)=1;
g1(25,74)=(-1);
g1(25,26)=params(59);
g1(26,19)=(-params(60));
g1(26,53)=1;
g1(26,75)=(-1);
g1(27,20)=(-params(61));
g1(27,54)=1;
g1(27,76)=(-1);
g1(28,21)=(-params(62));
g1(28,55)=1;
g1(28,77)=(-1);
g1(29,22)=(-params(63));
g1(29,56)=1;
g1(29,78)=(-1);
g1(30,23)=(-params(64));
g1(30,57)=1;
g1(30,79)=(-1);
g1(31,24)=(-params(65));
g1(31,58)=1;
g1(31,80)=(-1);
g1(32,25)=(-(1+params(66)));
g1(32,59)=1;
g1(32,81)=(-1);
g1(32,27)=params(66);
g1(33,18)=(-1);
g1(33,60)=1;
g1(34,25)=(-1);
g1(34,61)=1;

end
