function g1 = static_g1(T, y, x, params, T_flag)
% function g1 = static_g1(T, y, x, params, T_flag)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T         [#temp variables by 1]  double   vector of temporary terms to be filled by function
%   y         [M_.endo_nbr by 1]      double   vector of endogenous variables in declaration order
%   x         [M_.exo_nbr by 1]       double   vector of exogenous variables in declaration order
%   params    [M_.param_nbr by 1]     double   vector of parameter values in declaration order
%                                              to evaluate the model
%   T_flag    boolean                 boolean  flag saying whether or not to calculate temporary terms
%
% Output:
%   g1
%

if T_flag
    T = ftpl.static_g1_tt(T, y, x, params);
end
g1 = zeros(34, 34);
g1(1,19)=1;
g1(2,12)=(-1);
g1(2,20)=1;
g1(3,1)=1;
g1(3,7)=(-(T(2)*(T(1)/(1+T(1))+1/(1+T(1))-1)));
g1(3,19)=(-1);
g1(3,22)=(-T(2));
g1(4,3)=(-(T(3)*1/(params(6)*params(6)*params(26))));
g1(4,23)=(-1);
g1(5,3)=1-T(5);
g1(5,10)=(-T(4));
g1(5,12)=1;
g1(6,2)=(-params(17));
g1(6,4)=1-(1-params(17));
g1(6,23)=(-(params(26)*params(6)*params(6)*params(17)));
g1(7,1)=(-((1-params(37))/((1-params(37))*params(36))));
g1(7,5)=1;
g1(7,8)=(-((-(params(38)*(1-params(37))))/((1-params(37))*params(36))));
g1(7,24)=(-((-1)/((1-params(37))*params(36))));
g1(8,5)=(-(T(7)*(params(32)/(1+params(6)*params(14))-(1+params(6)*params(14)*params(32))/(1+params(6)*params(14)))));
g1(8,6)=(-(T(7)*(T(3)+T(8))));
g1(8,7)=(-(T(7)*T(6)*T(12)));
g1(8,9)=(-(T(7)*T(6)*params(30)));
g1(8,17)=1;
g1(8,19)=(-(T(7)*T(8)));
g1(8,27)=(-T(7));
g1(9,5)=1;
g1(9,7)=(-T(12));
g1(9,9)=(-params(30));
g1(9,18)=1;
g1(9,19)=(-1);
g1(9,28)=(-1);
g1(10,6)=1;
g1(10,17)=(-params(41));
g1(10,18)=(-(1-params(41)));
g1(11,2)=(-((-params(21))/params(22)));
g1(11,7)=1;
g1(11,8)=(-(1/params(22)));
g1(11,10)=(-((-(params(23)*T(10)))/params(22)));
g1(11,13)=(-((-0.01)/params(22)));
g1(11,21)=(-((-1)/params(22)));
g1(12,4)=(-(params(35)*params(40)));
g1(12,8)=1;
g1(12,9)=(-(params(35)*(1-params(40))));
g1(12,10)=(-(params(35)*T(10)*params(40)));
g1(12,25)=(-params(35));
g1(13,4)=(-1);
g1(13,6)=1;
g1(13,9)=1;
g1(13,10)=(-(1+T(10)));
g1(14,5)=(-(params(42)*(1+params(6)*params(14)*params(33)-params(33))/T(11)/params(40)));
g1(14,6)=(-(params(42)*(-(1-params(40)))/params(40)+(1-params(42))*(-((1-params(40))/params(40)))));
g1(14,10)=1;
g1(14,19)=(-(params(42)*(-(params(6)*params(14)))/T(11)/params(40)));
g1(14,25)=(-(params(42)*1/params(40)+(1-params(42))*1/params(40)));
g1(14,26)=(-(params(42)*(-(1+params(6)*params(14)*params(33)))/T(11)/params(40)));
g1(15,3)=(-params(43));
g1(15,4)=(-params(43));
g1(15,11)=1;
g1(15,13)=params(43);
g1(15,15)=params(46);
g1(15,29)=(-1);
g1(16,1)=(-1);
g1(16,11)=(-1);
g1(16,12)=1;
g1(16,19)=1;
g1(17,13)=0.01000000000000001;
g1(17,14)=(-1.7);
g1(17,20)=0.7;
g1(17,30)=(-1);
g1(18,3)=(-(T(5)-1));
g1(18,4)=T(4);
g1(18,8)=(-T(4));
g1(18,14)=1;
g1(19,31)=(-1);
g1(20,4)=(-1.17278);
g1(20,13)=0.245928;
g1(20,15)=(-0.073148);
g1(20,16)=1;
g1(21,21)=1-params(54);
g1(22,22)=1-params(56);
g1(23,23)=1-params(57);
g1(24,24)=1-params(58);
g1(25,25)=1-(1+params(59));
g1(25,33)=params(59);
g1(26,26)=1-params(60);
g1(27,27)=1-params(61);
g1(28,28)=1-params(62);
g1(29,29)=1-params(63);
g1(30,30)=1-params(64);
g1(31,31)=1-params(65);
g1(32,32)=1-(1+params(66));
g1(32,34)=params(66);
g1(33,25)=(-1);
g1(33,33)=1;
g1(34,32)=(-1);
g1(34,34)=1;

end
