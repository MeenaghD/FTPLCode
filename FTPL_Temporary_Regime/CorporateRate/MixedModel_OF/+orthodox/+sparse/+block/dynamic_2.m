function [y, T, residual, g1] = dynamic_2(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
residual=NaN(2, 1);
  residual(1)=(y(66))-(y(32)+params(66)*(y(32)-y(34)));
  residual(2)=(y(68))-(y(32));
if nargout > 3
    g1_v = NaN(2, 1);
g1_v(1)=1;
g1_v(2)=1;
    if ~isoctave && matlab_ver_less_than('9.8')
        sparse_rowval = double(sparse_rowval);
        sparse_colval = double(sparse_colval);
    end
    g1 = sparse(sparse_rowval, sparse_colval, g1_v, 2, 2);
end
end
