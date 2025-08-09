function [ YY, NKweight_output,nsims, NB_Tall, debtgdp2_all, all_coef25] = Boots_data_endog_switching_parallel_ZLB_ftpl(act_data,nvars,inno,inno_ZLB,nboot,start_lagged_pinf,coef,rho_est,rho_est_ZLB,NBbase,nomgdp)
%Input:
%	nboot:  number of bootstraps.
%	A:  oo_.dr.ghx
%	B:  oo_.dr.ghx
%   inno: model innovation
%Output:
%	YY:   k*T*nboot matrix
% load([fname '_results.mat']);
[~, T] = size(act_data);

YY = zeros(nvars, T-2, nboot);
NKweight_output = NaN(T-2, 3, nboot);  % note: nvars+1 was incorrect based on usage
NB_Tall=NaN(T-2, nboot);

inno = inno(:, sum(isnan(inno), 1) == 0);  % Remove NaNs
inno_ZLB = inno_ZLB(:, sum(isnan(inno_ZLB), 1) == 0);  % Remove NaNs

NBbase=exp(NBbase./100);

parfor i = 1:nboot
% for i = 1:nboot
    % if(rem(i,100)==0)
    %     disp(i)
    % end
    inno_local=inno;
    inno_ZLB_local=inno_ZLB;

    y = zeros(nvars, T);
    yforward = zeros(nvars, T + 49);
    lagged_pinf = NaN(T + 1 + 16, 1);
    lagged_pinf(1:16, 1) = start_lagged_pinf;
% Create a reproducible random stream unique to this iteration
    stream = RandStream('CombRecursive','Seed',5+i); 
    inx = randi(stream, size(inno_local,2), T+1, 1);
    % inx = randi(size(inno, 2), T + 1, 1);
    coef_local = coef;  % local copy for thread safety
    rho_est_local = rho_est;  % local copy for thread safety
    rho_est_local_ZLB = rho_est_ZLB;  % local copy for thread safety


    nk_temp = NaN(T, 3);  % <- Temporary local variable

    surplus=NaN(1,T+49);
    surplus(1,1:2)=act_data(32,1:2);
    NB=NaN(1,T+49);
    NB(1,1:2)=exp(act_data(33,1:2)./100);
    r5=NaN(1,T+49);
    pinf=NaN(1,T+49);
    NB_T=NaN(T,1);
    debtgdp2=NaN(T,1);
    coef25=NaN(T,1);
    for t = 3:T 
        mean_lagged_pinf = mean(lagged_pinf(1 + (t - 3):16 + (t - 3)));
        try
            coef_local(16) = exp(-coef_local(21) * abs(mean_lagged_pinf));
            coef_local(17) = exp(-coef_local(22) * abs(mean_lagged_pinf));
        catch
            coef_local(16)=0.7;
            coef_local(17)=0.7;
        end

        if t==3
            r5(1,1:T+49)=act_data(1,t);
            pinf(1,1:T+49)=act_data(5,t);
        else
            for t1=t:T+49
                r5(1,t1)=mean(yforward(1,t1:t1+20));
            end
            pinf(1,t:T+49)=yforward(5,t:T+49);
        end
        for t1 = t:T+49
            if t1==t
                surplus(1,t1)=surplus(1,t1-1)+rho_est_local(13)*(surplus(1,t1-1)-surplus(1,t1-2))+inno_local(12, inx(t));
            else
                surplus(1,t1)=surplus(1,t1-1)+rho_est_local(13)*(surplus(1,t1-1)-surplus(1,t1-2));
            end
            NB(1,t1)=NB(1,t1-1)-surplus(1,t1)+(r5(1,t1-1)/100)*(NB(1,t1-1)-20*((r5(1,t1)/100)-(r5(1,t1-1)/100))*NB(1,t1-1))-(0.075*(pinf(1,t-1)/100)*NB(1,t1-1));
        end
        % Catch crazy simulations.
        if abs(NB(1,t+49))>10000000
            y=zeros(nvars, T);
            break
        else
            P_T=0.02*NB(1,t+49)/(coef(26)*NBbase(1,t+49));
        end
        if P_T < 0
            P_T=0;
        end
        NB_T(t,:)=NB(1,t+49);
        % disp(['t=',num2str(t),' P_T=',num2str(P_T),' NB_T=',num2str(NB(1,t+49))]);
        % P_T=0.02*NB(1,t+49)/(coef(26));
        pinfbar=ones(1,size(inno_local,2)).*P_T/49;
        inno_local(13,:)=pinfbar;
        inno_ZLB_local(13,:)=pinfbar;

        debtgdp2(t,1)=(NB(1,t)/nomgdp(t,1))^2;
        if debtgdp2(t,1) <0
            coef_local(25)=0.0;
        else
            coef_local(25)=min(coef_local(24)*debtgdp2(t,1),1);
        end
        coef25(t,1)=coef_local(25);

        % oo_ = orthodox.driver_function(coef_local);
        [M_, oo_] = driver_function_orthodox_weighted(coef_local,rho_est_local);
        A=oo_.dr.ghx;
        B=oo_.dr.ghu;
        if isempty(A)
            y=zeros(nvars, T);
            break
        end
        y(oo_.dr.order_var, t) = A * y(oo_.dr.state_var, t - 1) + B * inno_local(:, inx(t));

        % extend forward for future periods simulations
        yforward(:,t)=y(:,t);

        for t1=t+1:T+70
            yforward(oo_.dr.order_var, t1) = A * yforward(oo_.dr.state_var, t1 - 1) ;
        end

        lagged_pinf(16 + (t - 2), 1) = y(5, t);
        nk_temp(t, :) = [mean_lagged_pinf coef_local(16) coef_local(17)];

        % ZLB cutoff from calc_ZLBcutoff.m
        if y(1,t)<-1.275
            y(1,t-1)=-1.275;
            [M_ZLB, oo_ZLB] = driver_function_orthodox_weighted_zlb(coef_local,rho_est_local_ZLB);
            A_ZLB=oo_ZLB.dr.ghx;
            B_ZLB=oo_ZLB.dr.ghu;

            y(oo_ZLB.dr.order_var, t) = A_ZLB * y(oo_ZLB.dr.state_var, t - 1) + B_ZLB * inno_ZLB_local(:, inx(t));

            % extend forward for future periods simulations
            yforward(:,t)=y(:,t);
            for t1=t+1:T+70
                yforward(oo_ZLB.dr.order_var, t1) = A_ZLB * yforward(oo_ZLB.dr.state_var, t1 - 1) ;
            end

            lagged_pinf(16 + (t - 1), 1) = y(5, t);
            nk_temp(t, :) = [mean_lagged_pinf coef_local(16) coef_local(17)];
        end            

    end

    YY(:, :, i) = y(:, 3:end);
    NKweight_output(:, :, i) = nk_temp(3:end,:);  % <- Assign after the loop
    NB_Tall(:,i)=NB_T(3:end,:);  
    debtgdp2_all(:,i)=debtgdp2(3:end,:);
    all_coef25(:,i)=coef25(3:end,:);
end

% take out crazy simulations
for i=nboot:-1:1
    if max(max(YY(:,:,i)))==0
        YY(:,:,i)=[];
        NKweight_output(:,:,i)=[];
        NB_Tall(:,i)=[];
        debtgdp2_all(:,i)=[];
        all_coef25(:,i)=[];
    end
end
nsims=size(YY,3);