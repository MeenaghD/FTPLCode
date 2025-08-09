% 1) Calculate the percentage of periods in the t-bill data that are in the ZLB.
% 2) Simulate the Orthodox model with t-bill and no ZLB 500 times. 
% 3) Collate all the simulated interest rates, and sort them.
% 4) Find the value that corresponds with the percentile in 1).
% This gives the ZLB cutoff, so a similar percent of periods will be in the ZLB.

load USdata_1959_2017.mat
ZLB_pc=(sum(data(:,1)<0.0625))/size(data,1);

%%
load EstResultsOrthodoxTbill.mat

all_intsims=[];
for i=1:size(boots_data,3)
    all_intsims=[all_intsims;boots_data(1,:,i)'];
end

s_all_intsims=sort(all_intsims);
indx=round(size(s_all_intsims,1)*ZLB_pc);


ZLBcutoff=s_all_intsims(indx,1)

