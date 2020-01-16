November 2019
# Data and code for the manuscript "Biological and statistical interpretation of size-at-age, mixed-effects models of growth"


## 1. Model fitting

The script `gomp_parall_tmb.r` reads tag-recapture data and fits the mixed-effects models in parallel pooling together all populations data. 

The algorithms to fit the models are called from the script `gomp_parall_tmb.r` and are in `scripts/gomp_vB_TMB_parall_validation_choice_rand_choice_cov.r` and in `scripts/vB_TMB_parall_validation_choice_rand_choice_cov.r` for the Gompertz and von Bertalanffy models, respectively.

The C++/TMB  code for fitting mixed-effects growth models are in `scripts/m_grow3_TMB_daily.cpp` for the von Bertalanffy growth function and `scripts/m_grow3_TMB_daily_gomp.cpp` for the Gompertz growth function. The script `gomp_parall_tmb_single.r` does what `gomp_parall_tmb.r` does, but for each population separately. 

Both routines launched with `gomp_parall_tmb.r` and `gomp_parall_tmb.r` are RAM intensive and they take quite a few hours, probably days, to run. 

You can reduce the number of replicates for each model (now set at 5) or the number of populations (now all 4) by changing a few parameters direcly in `gomp_parall_tmb.r` and `gomp_parall_tmb_single.r`.

## 2. Analysis of model fitting results    

Donwload the files in `https://figshare.com/articles/Modeling_results_for_growth_models_-_vBGF_and_Gompertz/10301678` (> 450 MB) and save them in the folder `data/`.

The script `Plot_traj.r` produces the plot (`Plots_growth/ Plot_tr_gr_all.pdf`) of observed trajectories for fish sampled in September.   

The script `Plot_wrong_pred.r` (1) plots the estimates of asymptotic size obtained with the same model, for either Gompertz or von Bertalanffy growth functions (output saved in `Plots_growth/Plot_corr_linf.jpg`); (2) finds the most consistently worst predicted individuals (ouput saved in `data/worst_pred.RDS`) and plots their trajectories with the trajectories of other individuals in the same population in the background (outoput saved in `Plots_growth/Plot_wrong_pred.jpg`); (3) Plot of observed and predicted trajectories for two individuals (output saved in `Plots_growth/Plot_pred_all.pdf`); (4) correlation between estimates of asymptotic size with the two growth functions with the model `mod_3_rand_l_Pop_k_Pop_t0_Pop` (output saved in `data/test_linf.RDS`). 

The scripts `Plots_growth.r`, after choosing a model in the script (e.g., `mod_3_rand_l_Species_k_Pop_t0_Pop` or `mod_3_rand_l_Const_k_Species_t0_Pop`), produces plots that show the distribution of asympotic size and k for all populations and each growth function, along with correlation plots of asymptotic size and k (output saved in `Plots_growth/Plot_dens.pdf`, `Plots_growth/Plot_linf.pdf`, `Plots_growth/Plot_k.pdf`, `Plots_growth/Plot_corr.pdf`).


## 3. Manuscript

The manuscript is currently under review. A pre-print can be found at https://www.biorxiv.org/content/10.1101/845222v1
