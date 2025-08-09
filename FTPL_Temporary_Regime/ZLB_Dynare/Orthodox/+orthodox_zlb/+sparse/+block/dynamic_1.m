function [y, T] = dynamic_1(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
  y(33)=y(1);
  y(53)=params(54)*y(21)+params(55)*x(5)+x(1);
  y(54)=params(56)*y(22)+x(2);
  y(55)=params(57)*y(23)+x(3);
  y(56)=params(58)*y(24)+x(4);
  y(58)=params(60)*y(26)+x(6);
  y(59)=params(61)*y(27)+x(7);
  y(60)=params(62)*y(28)+x(8);
  y(61)=params(63)*y(29)+x(9);
  y(62)=params(64)*y(30)+x(10);
  y(63)=params(65)*y(31)+x(11);
end
