function [ YY, NKweight_output] = Boots_data_endog_switching_parallel_mix(act_dataPre,act_dataPost,nvarsPre,nvarsPost,innoPre,innoPost,nboot,start_lagged_pinf,coefPre,coefPost,rho_estPre,rho_estPost,NBbase)
%Input:
%	nboot:  number of bootstraps.
%	A:  oo_.dr.ghx
%	B:  oo_.dr.ghx
%   inno: model innovation
%Output:
%	YY:   k*T*nboot matrix
% load([fname '_results.mat']);
[~, TPre] = size(act_dataPre);
[~, TPost] = size(act_dataPost);
T=TPre+TPost;

YY = zeros(nvarsPost, T-1, nboot);
NKweight_output = NaN(T-1, 3, nboot);  % note: nvars+1 was incorrect based on usage
NB_Tall=NaN(T-2, nboot);

innoPre = innoPre(:, sum(isnan(innoPre), 1) == 0);  % Remove NaNs
innoPost = innoPost(:, sum(isnan(innoPost), 1) == 0);  % Remove NaNs
n_innoPre=size(innoPre,2);
n_innoPost=size(innoPost,2);

NBbase_exp=exp(NBbase./100);

% Set up independent streams for each worker
if isempty(gcp('nocreate'))
    parpool('local');
end
rng('shuffle');   % shuffle seed based on time
parfor i = 1:nboot

    y = zeros(nvarsPre, T);
    yforward = zeros(nvarsPost, T + 49);
    lagged_pinf = NaN(T + 1 + 16, 1);
    lagged_pinf(1:16, 1) = start_lagged_pinf;
    % Create a reproducible random stream unique to this iteration
    stream = RandStream('CombRecursive','Seed',1+i); 
    % stream = RandStream('CombRecursive','Seed',sum(1000*clock) + i);
    inxPre = randi(stream, n_innoPre, T+1, 1);
    inxPost = randi(stream, n_innoPost, T+1, 1);
    % inxPre = randi(n_innoPre, T+1, 1);
    % inxPost = randi(n_innoPost, T+1, 1);
    coefPre_local = coefPre;  % local copy for thread safety
    rho_estPre_local = rho_estPre;  % local copy for thread safety
    coefPost_local = coefPost;  % local copy for thread safety
    rho_estPost_local = rho_estPost;  % local copy for thread safety
    innoPre_local = innoPre;  % local copy for thread safety
    innoPost_local = innoPost;  % local copy for thread safety
    NB_T_local=NaN(T,1);

    nk_temp = NaN(T, 3);  % <- Temporary local variable

    surplus = NaN(1, T + 49);  % Allocate sufficient size
    NB = NaN(1, T + 49);
    r5 = NaN(1, T + 49);
    pinf = NaN(1, T + 49);
    surplus(1,TPre-1:TPre)=act_dataPost(32,1:2);
    NB(1,TPre-1:TPre)=exp(act_dataPost(33,1:2)./100);
    NBbase_local=NBbase_exp;

    for t = 2:TPre
        mean_lagged_pinf = mean(lagged_pinf(1 + (t - 2):16 + (t - 2)).^2);
        coefPre_local(16) = exp(-coefPre_local(21) * abs(mean_lagged_pinf));
        coefPre_local(17) = exp(-coefPre_local(22) * abs(mean_lagged_pinf));
        % oo_ = orthodox.driver_function(coef_local);
        [M_Pre, oo_Pre] = driver_function_orthodox(coefPre_local,rho_estPre_local);
        APre=oo_Pre.dr.ghx;
        BPre=oo_Pre.dr.ghu;

        y(oo_Pre.dr.order_var, t) = APre * y(oo_Pre.dr.state_var, t - 1) + BPre * innoPre_local(:, inxPre(t));
        lagged_pinf(16 + (t - 1), 1) = y(5, t);
        nk_temp(t, :) = [mean_lagged_pinf coefPre_local(16) coefPre_local(17)];
    end

    for t=TPre+1:T
        mean_lagged_pinf = mean(lagged_pinf(1 + (t - 2):16 + (t - 2)).^2);
        try
            coefPost_local(16) = exp(-coefPost_local(21) * abs(mean_lagged_pinf));
            coefPost_local(17) = exp(-coefPost_local(22) * abs(mean_lagged_pinf));
        catch
            coefPost_local(16)=0.7;
            coefPost_local(17)=0.7;
        end
        % oo_ = orthodox.driver_function(coef_local);
        [M_Post, oo_Post] = driver_function_ftpl(coefPost_local,rho_estPost_local);
        APost=oo_Post.dr.ghx;
        BPost=oo_Post.dr.ghu;

        if t==TPre+1
            r5(1,1:T+49)=y(1,t);
            pinf(1,1:T+49)=y(5,t);
        else
            for t1=t:T+49
                r5(1,t1)=mean(yforward(1,t1:t1+20));
            end
            pinf(1,t:T+49)=yforward(5,t:T+49);
        end
        for t1 = t:T+49
            if t1==t
                surplus(1,t1)=surplus(1,t1-1)+rho_estPost_local(13)*(surplus(1,t1-1)-surplus(1,t1-2))+innoPost_local(12, inxPost(t));
            else
                surplus(1,t1)=surplus(1,t1-1)+rho_estPost_local(13)*(surplus(1,t1-1)-surplus(1,t1-2));
            end
            NB(1,t1)=NB(1,t1-1)-surplus(1,t1)+(r5(1,t1-1)/100)*(NB(1,t1-1)-20*((r5(1,t1)/100)-(r5(1,t1-1)/100))*NB(1,t1-1))-(0.075*(pinf(1,t-1)/100)*NB(1,t1-1));
        end

        % Catch crazy simulations.
        if abs(NB(1,t+49))>10000000
            y=zeros(nvarsPost, T);
            break
        else
            P_T=0.02*NB(1,t+49)/(coefPost(26)*NBbase_local(1,t+49));
        end
        if P_T < 0
            P_T=0;
        end
        NB_T_local(t)=NB(1,t+49);
        % disp(['t=',num2str(t),' P_T=',num2str(P_T),' NB_T=',num2str(NB(1,t+49))]);
        % P_T=0.02*NB(1,t+49)/(coef(26));
        pinfbar=ones(1,n_innoPost).*P_T/49;
        innoPost_local(13,:)=pinfbar;

        y(oo_Post.dr.order_var, t) = APost * y(oo_Post.dr.state_var, t - 1) + BPost * innoPost_local(:, inxPost(t));

        % extend forward for future periods simulations
        yforward(:,t)=y(:,t);
        for t1=t+1:T+70
            yforward(oo_Post.dr.order_var, t1) = APost * yforward(oo_Post.dr.state_var, t1 - 1) ;
        end
        lagged_pinf(16 + (t - 1), 1) = y(5, t);
        nk_temp(t, :) = [mean_lagged_pinf coefPost_local(16) coefPost_local(17)];

    end
    YY(:, :, i) = y(:, 2:end);
    NKweight_output(:, :, i) = nk_temp(2:end,:);  % <- Assign after the loop
    NB_Tall(:,i)=NB_T_local(3:end,:);    
end

end
