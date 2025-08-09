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
    T = ftpl.dynamic_g1_tt(T, y, x, params, steady_state, it_);
end
g1 = zeros(33, 79);
g1(1,45)=1;
g1(1,79)=(-1);
g1(2,66)=(-1);
g1(2,46)=1;
g1(3,27)=1;
g1(3,7)=(-(T(2)*T(1)/(1+T(1))));
g1(3,33)=T(2);
g1(3,63)=(-(T(2)*1/(1+T(1))));
g1(3,35)=(-(T(2)*T(3)));
g1(3,64)=(-(T(2)*(-T(3))));
g1(3,45)=(-1);
g1(3,48)=(-T(2));
g1(4,2)=(-T(4));
g1(4,28)=1;
g1(4,60)=(-(params(6)*params(14)*T(4)));
g1(4,29)=(-(T(4)*1/(params(6)*params(6)*params(26))));
g1(4,49)=(-1);
g1(5,29)=1;
g1(5,61)=(-T(6));
g1(5,65)=(-T(5));
g1(5,66)=1;
g1(6,28)=(-params(17));
g1(6,4)=(-(1-params(17)));
g1(6,30)=1;
g1(6,49)=(-(params(26)*params(6)*params(6)*params(17)));
g1(7,1)=(-((-params(37))/((1-params(37))*params(36))));
g1(7,27)=(-(1/((1-params(37))*params(36))));
g1(7,31)=1;
g1(7,8)=(-(params(39)/((1-params(37))*params(36))));
g1(7,34)=(-(((-(params(38)*(1-params(37))))-params(39))/((1-params(37))*params(36))));
g1(7,50)=(-((-1)/((1-params(37))*params(36))));
g1(8,5)=(-(T(8)*params(32)/(1+params(6)*params(14))));
g1(8,31)=(-(T(8)*(-((1+params(6)*params(14)*params(32))/(1+params(6)*params(14))))));
g1(8,6)=(-(T(4)*T(8)));
g1(8,62)=(-(T(8)*T(9)));
g1(8,7)=(-(T(8)*T(7)*(-(T(1)/(1-T(1))))));
g1(8,33)=(-(T(8)*T(7)*1/(1-T(1))));
g1(8,35)=(-(T(8)*T(7)*params(30)));
g1(8,43)=1;
g1(8,45)=(-(T(8)*T(9)));
g1(8,53)=(-T(8));
g1(9,31)=1;
g1(9,7)=T(1)/(1-T(1));
g1(9,33)=(-(1/(1-T(1))));
g1(9,35)=(-params(30));
g1(9,44)=1;
g1(9,45)=(-1);
g1(9,54)=(-1);
g1(10,32)=1;
g1(10,43)=(-params(41));
g1(10,44)=(-(1-params(41)));
g1(11,28)=(-((-params(21))/params(22)));
g1(11,33)=1;
g1(11,34)=(-(1/params(22)));
g1(11,36)=(-((-(params(23)*T(11)))/params(22)));
g1(11,39)=(-((-0.01)/params(22)));
g1(11,47)=(-((-1)/params(22)));
g1(12,4)=(-(params(35)*params(40)));
g1(12,34)=1;
g1(12,35)=(-(params(35)*(1-params(40))));
g1(12,36)=(-(params(35)*T(11)*params(40)));
g1(12,51)=(-params(35));
g1(13,4)=(-1);
g1(13,32)=1;
g1(13,35)=1;
g1(13,36)=(-(1+T(11)));
g1(14,5)=(-(params(42)*(-params(33))/T(12)/params(40)));
g1(14,31)=(-(params(42)*(1+params(6)*params(14)*params(33))/T(12)/params(40)));
g1(14,32)=(-(params(42)*(-(1-params(40)))/params(40)+(1-params(42))*(-((1-params(40))/params(40)))));
g1(14,36)=1;
g1(14,45)=(-(params(42)*(-(params(6)*params(14)))/T(12)/params(40)));
g1(14,51)=(-(params(42)*1/params(40)+(1-params(42))*1/params(40)));
g1(14,52)=(-(params(42)*(-(1+params(6)*params(14)*params(33)))/T(12)/params(40)));
g1(15,29)=(-params(43));
g1(15,30)=(-params(43));
g1(15,37)=1;
g1(15,39)=params(43);
g1(15,41)=params(46);
g1(15,55)=(-1);
g1(16,1)=(-1);
g1(16,9)=(-1);
g1(16,38)=1;
g1(16,45)=1;
g1(17,10)=(-0.99);
g1(17,39)=1;
g1(17,40)=(-1.7);
g1(17,13)=0.7;
g1(17,56)=(-1);
g1(18,3)=1;
g1(18,29)=(-T(6));
g1(18,30)=T(5);
g1(18,34)=(-T(5));
g1(18,40)=1;
g1(19,11)=(-1);
g1(19,41)=1;
g1(19,12)=params(45);
g1(19,42)=(-params(45));
g1(19,57)=(-1);
g1(20,30)=(-1.17278);
g1(20,39)=0.245928;
g1(20,41)=(-0.073148);
g1(20,42)=1;
g1(21,14)=(-params(54));
g1(21,47)=1;
g1(21,67)=(-1);
g1(21,71)=(-params(55));
g1(22,15)=(-params(56));
g1(22,48)=1;
g1(22,68)=(-1);
g1(23,16)=(-params(57));
g1(23,49)=1;
g1(23,69)=(-1);
g1(24,17)=(-params(58));
g1(24,50)=1;
g1(24,70)=(-1);
g1(25,18)=(-(1+params(59)));
g1(25,51)=1;
g1(25,71)=(-1);
g1(25,26)=params(59);
g1(26,19)=(-params(60));
g1(26,52)=1;
g1(26,72)=(-1);
g1(27,20)=(-params(61));
g1(27,53)=1;
g1(27,73)=(-1);
g1(28,21)=(-params(62));
g1(28,54)=1;
g1(28,74)=(-1);
g1(29,22)=(-params(63));
g1(29,55)=1;
g1(29,75)=(-1);
g1(30,23)=(-params(64));
g1(30,56)=1;
g1(30,76)=(-1);
g1(31,24)=(-params(65));
g1(31,57)=1;
g1(31,77)=(-1);
g1(32,25)=(-params(66));
g1(32,58)=1;
g1(32,78)=(-1);
g1(33,18)=(-1);
g1(33,59)=1;

end
