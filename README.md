March 2020

# Data and code for the manuscript "Biological and statistical interpretation of size-at-age, mixed-effects models of growth", to be published in the Journal of the Royal Society Open Science


<strong>Here is the abstract of the paper, which gives context to the modeling done.</strong>

> The differences in life-history traits and processes between organisms living in the same or different populations contribute to their ecological and evolutionary dynamics.  
We developed mixed-effect model formulations of the popular size-at-age von Bertalanffy and Gompertz growth functions to estimate individual and group variation in body growth, using as a model system four freshwater fish populations, where tagged individuals were sampled for more than 10 years. We used the software Template Model Builder to estimate the parameters of the mixed effect growth models.  
Tests on data that were not used to estimate model parameters showed good predictions of individual growth trajectories using the mixed-effects models and starting from one single observation of body size early in life; the best models had R2 > 0.80 over more than 500 predictions. Estimates of asymptotic size from the Gompertz and von Bertalanffy models were not significantly correlated, but their predictions of size-at-age of individuals were strongly correlated (r > 0.99), which suggests that choosing between the best models of the two growth functions would have negligible effects on the predictions of size-at-age of individuals. Model results pointed to size ranks that are largely maintained throughout the lifetime of individuals in all populations.


## 1. Model fitting

I ran the scripts and models with R version 3.6.1 (2019-07-05) for Mac. The parallel routine uses mclapply, which works only on Mac and Linux machines. In the self-contained short example (see Section 3, script `self_contained.r`), I included the code for fitting the models sequentially, which will work on Windows machines.

The script `gomp_parall_tmb.r` install the libraries needed, reads tag-recapture data, and fits the mixed-effects models in parallel pooling together the data for all four populations. 

The algorithms to fit the models are called from the script `gomp_parall_tmb.r` and are in `scripts/gomp_vB_TMB_parall_validation_choice_rand_choice_cov.r` and in `scripts/vB_TMB_parall_validation_choice_rand_choice_cov.r` for the Gompertz and von Bertalanffy models, respectively.

The C++/TMB  code for fitting mixed-effects growth models are in `scripts/m_grow3_TMB_daily.cpp` for the von Bertalanffy growth function and `scripts/m_grow3_TMB_daily_gomp.cpp` for the Gompertz growth function. The script `gomp_parall_tmb_single.r` does what `gomp_parall_tmb.r` does, but for each population separately. 

Both routines launched with `gomp_parall_tmb.r` and `gomp_parall_tmb.r` are RAM intensive and they take quite a few hours, and likely days on most laptops and desktops, to run. 

You can reduce the number of replicates for each model (now set at 5) or the number of populations (now all 4) by changing a few parameters directly in `gomp_parall_tmb.r` and `gomp_parall_tmb_single.r`.

The objects created and saved as .RDS are:  
- Data frame with read ID (Mark_cor), fake ID (Mark_ind) that avoids the problem of same tag used in different population, and Population (Pop) (`data/mark_all_pop_df.RDS`)  
- Data frame with sampling data for all populations `data/all_pop_df.RDS`. Columns are: Population, Species, ID[Mark], Sampling Date, Run [whether the individuals was captured after the first or second electrofishing pass], Sector [location within streams], Length, Weight, Cohort [year of birth].
- Lists with the results of model fitting (`ll_list_temp_gomp.RDS` and `ll_list_temp_vb.RDS` when there is validation and `ll_list_temp_gomp_all_data.RDS` and `ll_list_temp_vb_all_data.RDS` with no validation)  
- Estimates and predictions for model parameters for each individual in the data set `data/pred_df.RDS`.
- Model performance (`data/avg_rsq_df.RDS` in the case with validation and `data/avg_rsq_df_all.RDS` with no validation)

## 2. Analysis of model fitting results    

Donwload the .RDS files in <https://figshare.com/articles/Modeling_results_for_growth_models_-_vBGF_and_Gompertz/10301678> (total size > 450 MB) that have the already run results from `gomp_parall_tmb.r` and save them in the folder `data/`.

The script `Plot_traj.r` -- `source("Plot_traj.r")` -- produces the plot of observed trajectories for fish sampled in September. The figure is saved in `Plots_growth/ Plot_tr_gr_all.pdf`.  

![Plot_traj](https://github.com/simonevincenzi/Growth_Models/blob/master/Plots_growth/Plot_tr_gr_all_.png)

The script `Plot_wrong_pred.r` (1) plots the estimates of asymptotic size obtained with the same model (24 models total, 12 for either growth function), for either Gompertz or von Bertalanffy growth functions (the figure is saved in `Plots_growth/Plot_corr_linf.jpg`); 

![Plot_corr_linf](https://github.com/simonevincenzi/Growth_Models/blob/master/Plots_growth/Plot_corr_linf.jpg)

(2) finds the most consistently worst predicted individuals (data frame saved in `data/worst_pred.RDS`) and plots their trajectories with the trajectories of other individuals in the same population in the background (figure saved in `Plots_growth/Plot_wrong_pred.jpg`); 

![Plot_wrong_pred](https://github.com/simonevincenzi/Growth_Models/blob/master/Plots_growth/Plot_wrong_pred.jpg)

(3) Plot of observed and predicted trajectories for two individuals (figure saved in `Plots_growth/Plot_pred_all.pdf`); 

![Plot_pred](https://github.com/simonevincenzi/Growth_Models/blob/master/Plots_growth/Plot_pred_all.png)

(4) saves in a data frame the correlation between estimates of asymptotic size estimated for the two growth functions for the model with Population as predictor of the 3 parameters `mod_3_rand_l_Pop_k_Pop_t0_Pop` (data frame saved in `data/test_linf.RDS`). 

The scripts `Plots_growth.r`, after choosing a model directly in the script (e.g., `mod_3_rand_l_Pop_k_Pop_t0_Pop` or `mod_3_rand_l_Const_k_Species_t0_Pop`), produces plots that show the distribution of asympotic size and k for all populations and each growth function, along with correlation plots of asymptotic size and k (figures saved in `Plots_growth/Plot_dens.pdf`, `Plots_growth/Plot_linf.pdf`, `Plots_growth/Plot_k.pdf`, `Plots_growth/Plot_corr.pdf`). Here below, I show the distribution of asymptotic siz (top figure) and the correlation between asymptotic size and growth rate (bottom figure) for the model `mod_3_rand_l_Pop_k_Pop_t0_Pop`.

![Plot_distr_l_inf](https://github.com/simonevincenzi/Growth_Models/blob/master/Plots_growth/Plot_linf_first.png)

![Plot_corr](https://github.com/simonevincenzi/Growth_Models/blob/master/Plots_growth/Plot_corr_first.png)

## 3. Self-contained short example

The script `self_contained.r` reads tag-recapture data and fits two mixed-effects models for vBGF and GGF (two replicates for each) in parallel pooling together all populations data. Then, it creates a data frame with metrics of models performance (avg_rsq_df), train (train_df) and test (test_df) datasets that include both raw data and model predictions, and a list (plot_list) with the same plots as in the manuscript (excluding (a) raw trajectories and (b) the correlation between asymptotic size and growth rate is only for the model with Pop as predictor of all 3 parameters). 

On my 2018 MacBookPro with 16 GB 2400 MHz DDR4 of RAM and 2.6 GHz Intel Core i7 CPU, it takes approximately 30 mins to run the script (the fitting itself is quite fast, but the predictions and their standard errors take much longer).


## 4. Manuscript

The manuscript will be published in the Journal of the Royal Society Open Science. A pre-print can be found at https://www.biorxiv.org/content/10.1101/845222v1.
