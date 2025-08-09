log_all=[];
for i=1:6
    filename=['log_bestmax_40pc_',num2str(i)];
    load(filename);
    log_all=[log_all;output_table];
end

save log_bestmax_40pc_all log_all

%%