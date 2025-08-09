# FTPL-Paper
This repository contains the model files for the paper “Does the Fiscal Theory of the Price Level Explain US Postwar Behaviour” by Vo Phuong Mai Le, David Meenagh, Patrick Minford and Michael Wickens. A working paper version can be found at http://carbsecon.com/wp/E2024_17.pdf

The folder FTPL_Permanent_Regime contains the code for the models when the FTPL is considered a permanent regime. The folders contain the executables, which have been compiled to work on Windows. 

The folder FTPL_Temporary_Regime contains the Matlab codes using Dynare for the models when the FTPL is considered a temporary regime.

**FTPL_Permanent_Regime** 

There are 2 folders
1. Main_Files: This contains the files needed to run the NK model with the ZLB
2. Stable_pistar: This contains the files needed stable path results

Main_Files:
To simulate and test the NK model with ZLB you need to run several scripts in the following order:
1.	calc_base_script_nocrisis
2.	calc_base_script_crisis
3.	bootstrap_script_surplus_ZLB
4.	calc_wald_script

The main model files are:
ftpl_base_nocrisis.f and ftpl_sims_nocrisis.f
ftpl_base_crisis.f and ftpl_sims_crisis.f

The nocrisis files are the main model for normal times, the crisis files are when the ZLB holds. 

The base and sims files are the same except the base file solves the model with no shocks and writes residuals to make sure the model solves to the steady state when there are no shocks. The sim file then reads these residuals. The model equations can be found in SUBROUTINE FTPL

ftpl_base.data and ftpl_sims.data are the data files used as inputs to the model files. The data in these files is a steady state. The data average for stationary data, and the average trend for nonstationary data. 

calc_shocks.f90
This file reads the actual data from act_data.data and calculates the model residuals and innovations that are consistent with the model. These innovations are used to bootstrap the model. 

Stable_pistar:
The files follow the same structure as in Main_Files. The only difference is that a stable long run inflation is imposed on the model. In Table 2 we use 5 different Debt Implied Inflations (1%, 5%, 10%, 15%, 20%). To replicate each of these replace PISTAR in SUBROUTINE FTPL with the required value. 

**FTPL_Temporary_Regime**

This folder contains the various different models (Orthodox, FTPL, Orthodox Pre-GFC/FTPL Post-GFC, FTPL Pre-GFC/Orthodox Post-GFC, MUR) for the two different specifications (Corporate Rate, ZLB). 

Within each subfolder, the file II_Wald.m will test the model and return the Wald statistic and the p-value. 
