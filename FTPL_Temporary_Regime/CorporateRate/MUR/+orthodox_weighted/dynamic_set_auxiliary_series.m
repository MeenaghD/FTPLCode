function ds = dynamic_set_auxiliary_series(ds, params)
%
% Computes auxiliary variables of the dynamic model
%
ds.AUX_ENDO_LAG_24_1=ds.SH_4(-1);
ds.AUX_ENDO_LAG_31_1=ds.surplus(-1);
end
