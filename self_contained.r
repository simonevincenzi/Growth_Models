######### SELF-CONTAINED EXAMPLE OF MODEL FITTING AND DOWNSTREAM ANALYSES AND PLOTS ##############



##### Load libraries

if(!require(pacman))install.packages("pacman")
pacman::p_load("tidyverse", "data.table", "parallel", "TMB", "lubridate", "devtools", 
               "Metrics", "rlist","withr",
               "cowplot", "ggrepel")


library(tidyverse)
library(data.table)
library(parallel)
library(TMB)
library(lubridate)
library(devtools)
devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
library(TMBhelper)
library(Metrics)
library(rlist)
library(withr)
library(cowplot)
library(ggrepel)


#### Read and manipulate data ####

#### Lower Idrijca - Marble


loidri_df =  fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/loidri_df_pieced.csv") 
loidri_df$Date = as.Date(loidri_df$Date,format = "%m/%d/%Y") # Y is year with century
loidri_df = loidri_df %>%
  arrange(.,Mark_cor,Date) %>% 
  filter(., !is.na(Mark_cor))

loidri_mark_map = loidri_df %>% distinct(Mark_cor)

loidri_mark_map$Mark_ind = seq(1000,(1000 + nrow(loidri_mark_map) - 1),1)

loidri_mark_map$Pop = "LIdri_MT"

loidri_df = loidri_df %>% left_join(., loidri_mark_map)

loidri_df = dplyr::select(loidri_df, Species, Mark_ind, Date, Year, Month, Run, Sector, Length, Weight, Sex, Cohort_cor, Age_cor, Stream, Pop, Pop_ind) %>% 
  rename(., Cohort = Cohort_cor,
         Age = Age_cor,
         Mark = Mark_ind)


#### Upper Idrijca - Marble


uppidri_df =  fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/uppidri_df_pieced.csv")


uppidri_df$Date = as.Date(uppidri_df$Date,format = "%m/%d/%Y") # Y is year with century
uppidri_df = uppidri_df %>%
  arrange(.,Mark_cor,Date) %>% 
  filter(., !is.na(Mark_cor))

uppidri_mark_map = uppidri_df %>% distinct(Mark_cor)

uppidri_mark_map$Mark_ind = seq(3000,(3000 + nrow(uppidri_mark_map) - 1),1)

uppidri_mark_map$Pop = "UIdri_MT"

uppidri_df = uppidri_df %>% left_join(., uppidri_mark_map)


uppidri_df = dplyr::select(uppidri_df, Species, Mark_ind, Date, Year, Month, Run, Sector, Length, Weight, Sex, Cohort_cor, Age_cor, Stream, Pop, Pop_ind) %>% 
  rename(., Cohort = Cohort_cor,
         Age = Age_cor,
         Mark = Mark_ind)

uppidri_df = filter(uppidri_df, !Mark %in% unique(filter(uppidri_df, Age < 0)$Mark))

###


#### Lower Idrijca - Rainbow trout


rtidri_df = fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/rtidri_df_pieced.csv")

rtidri_df$Date = as.Date(rtidri_df$Date,format = "%m/%d/%Y")

rtidri_df = rtidri_df %>% 
  arrange(.,Mark_cor,Date) %>% 
  filter(., !is.na(Mark_cor))

rtidri_mark_map = rtidri_df %>% distinct(Mark_cor)

rtidri_mark_map$Mark_ind = seq(5000,(5000 + nrow(rtidri_mark_map) - 1),1)

rtidri_mark_map$Pop =  "LIdri_RT"

rtidri_df = rtidri_df %>% left_join(., rtidri_mark_map)

rtidri_df = dplyr::select(rtidri_df, Species, Mark_ind, Date, Year, Month, Run, Sector, Length, Weight, Sex, Cohort_cor, Age_cor, Stream, Pop, Pop_ind) %>% 
  rename(., 
         Cohort = Cohort_cor,
         Age = Age_cor,
         Mark = Mark_ind)


#### Upper Volaja - Brown


uppvol_df = fread("https://raw.githubusercontent.com/simonevincenzi/Heter/master/raw_data/uppvol_2015_complete.csv")

uppvol_df$Date = as.Date(uppvol_df$Date,format = "%m/%d/%y")

uppvol_df = uppvol_df %>% 
  mutate(., Mark_cor = Mark) %>% 
  arrange(.,Mark_cor,Date) %>% 
  filter(., !is.na(Mark_cor))


uppvol_df$Pop = "UVol_BT"
uppvol_df$Stream = "UVol"
uppvol_df$Species = "BT"
uppvol_df$Pop_ind = 4

uppvol_mark_map = uppvol_df %>% distinct(Mark_cor)

uppvol_mark_map$Mark_ind = seq(8000,(8000 + nrow(uppvol_mark_map) - 1),1)


uppvol_mark_map$Pop =  "UVol_BT"

uppvol_df = uppvol_df %>% left_join(., uppvol_mark_map)

uppvol_mark_map$Mark_cor = as.character(uppvol_mark_map$Mark_cor)

uppvol_df = dplyr::select(uppvol_df, Species, Mark_ind, Date, Year, Month, Run, Cohort,Sector, Length, Weight, Age,Sex, Stream, Pop, Pop_ind) %>% 
  rename(.,
         Mark = Mark_ind)




#### Mapping original Marks (e.g., nuumber on tags) to made-up marks to avoid same Mark (e.g., number on tags) in different populations


mark_all_pop_df = bind_rows(loidri_mark_map,
                            uppidri_mark_map,
                            rtidri_mark_map,
                            uppvol_mark_map)


# saveRDS(mark_all_pop_df, "data/mark_all_pop_df.RDS")

all_pop_df = bind_rows(loidri_df,
                       uppidri_df,
                       rtidri_df,
                       uppvol_df) %>% filter(., !is.na(Cohort), Age > 0 ,
                                             !is.na(Length), Length > 0, Month == 9)

# saveRDS(all_pop_df, "data/all_pop_df.RDS")

long_all_pop_df = all_pop_df %>% group_by(Mark,Pop) %>% summarize(n = n(), min_age = min(Age)) %>% 
  rename(Mark_ind = Mark) %>%  left_join(., mark_all_pop_df)



# Find the fish with the longest time series (min 3 observations) and use them for prediction
# Since fitting these models is computationally expensive, I fit them in parallel using a Mac 2018 with 2.6 GHz Intel Core i7 and 16 GB 2400 MHz DDR4 of memory. I use the function mclapply for parallel computation and I need to save the training and test data in lists 

validation = 1  # 0 is no validation, 1 is validation on test data set
data_region_list_full  = list() # initialize the list with input data

if (validation == 1) {
  seed_v = c(11,156)}  # seeds for model replicates (1 seed = 1 replicate) when doing validation 
if (validation == 0) {
  seed_v = c(5)} # # with no validation (all data is used for fitting), the seed is just for the random initialization

  cont = 1

for (ss in 1:length(seed_v)) {

  set.seed(seed_v[ss])
  
  valid_mark = long_all_pop_df %>% filter(., n>=3) %>%  group_by(Pop) %>%  # indentify the IDs for valication 
               sample_frac(.,.3) %>% rename(Mark = Mark_ind, Age = min_age)
  
  valid_mark$mark_age_kept = 1  # first measure of IDs in the validation data set
  valid_mark$mark_valid = 1 # ID in the validation data set
  
  all_pop_df_loo = all_pop_df %>% left_join(., select(valid_mark, Mark, Pop, Age, mark_age_kept)) %>% 
    left_join(., select(valid_mark, Mark, Pop, mark_valid))
  
  
  #### Set of models to be tested
  
  model_v = c("Const","Species + Pop + Cohort","Species * Pop * Cohort",
              "Pop","Species + Pop", "Species * Pop","Pop + Cohort",
              "Pop * Cohort","Species","Cohort")
  
  mod_comb_df = tibble(linf = model_v, k = model_v, t0 = model_v)
  
  mod_comb_df= expand.grid(mod_comb_df) %>% filter(., t0 %in% c("Const","Pop"), k %in% c("Const","Pop"))
  
  mod_comb_df$mod_id = 1:nrow(mod_comb_df)
  
  num_mods = nrow(mod_comb_df)
  
  data_region_list  = list()
  
  for (i in 1:nrow(mod_comb_df)) {
    data_region_list[[1]] = as.list(data.frame(data_region_df = NA, age_cut = NA, cont = NA, data_compl = NA))
    data_region_list[[1]]$data_region_df =  as.data.frame(all_pop_df_loo)
    data_region_list[[1]]$mark_all_pop_df = as.data.frame(mark_all_pop_df)
    data_region_list[[1]]$age_cut =  0
    data_region_list[[1]]$cont =  cont
    data_region_list[[1]]$data_compl =  "full"
    
    model_linf = mod_comb_df$linf[i]
    model_k = mod_comb_df$k[i]
    model_t0 = mod_comb_df$t0[i]
    
    data_region_list[[1]]$rand_eff_n = 3
    data_region_list[[1]]$linf_var = as.character(model_linf)
    data_region_list[[1]]$k_var = as.character(model_k)
    data_region_list[[1]]$t0_var = as.character(model_t0)
    data_region_list[[1]]$mod_id = i
    data_region_list[[1]]$valid = validation
    data_region_list[[1]]$seed = seed_v[ss]
    
    
    
    data_region_list_full = c(data_region_list_full, data_region_list)
    
    cont = cont + 1
    
  }
  
  
  
}


#### Filter input data only for models in this self_contained quick modeling
  
repl = length(data_region_list_full)/num_mods # how many seeds for validation replicates

if (repl>1) {

targ_mods = c(1,34) ## these could be changed - now 1 is Constnat predictors, and 34 is Pop as predictor for all 3 parameters
len_targ_mods = length(targ_mods)

targ_list = c(targ_mods, rep(0, len_targ_mods * (repl-1)))

for (i in 1:(repl-1)) {
  
  for(j in 1:len_targ_mods) {

targ_list[j + (len_targ_mods*i)] = targ_list[j] + (num_mods*i) 

}

}

data_region_list_full = data_region_list_full[targ_list]

} else {
  
  data_region_list_full = data_region_list_full[c(1,34)] ## if there is no validation (e.g,, full data set used)
    
}


####

#### Read in the files with the TMB algos and collections of results from model fitting

source("gomp_vB_TMB_parall_validation_choice_rand_choice_cov.r")
source("vB_TMB_parall_validation_choice_rand_choice_cov.r")

logFile = "log_file.txt"
cat("This is a log file for vB models", file=logFile, append=FALSE, sep = "\n")

#### Loop for parallel computations. I split the parallel computation in groups of "max" and save model results along the way
#### it uses the function mclapply, which works only on Mac and Linux. parLapply is a fairly equivalent implementation for Windows 
max <- 8 
x <- 1:length(data_region_list_full)
d1 <- split(1:length(data_region_list_full), ceiling(x/max))


ll_list_temp = list()
for (i in 1:length(d1)) {
  ll_list_temp_chunk = mclapply(d1[[i]],function (x) do.call(gomp_vB_TMB_parall_validation_choice_rand_choice_cov.f,data_region_list_full[[x]]), mc.cores = 8, mc.preschedule = F)
  ll_list_temp = c(ll_list_temp,ll_list_temp_chunk)
  rm(ll_list_temp_chunk)
  if (validation == 1) {
    saveRDS(ll_list_temp,"ll_list_temp_gomp_subset.RDS") }
  if (validation == 0) {
    saveRDS(ll_list_temp,"ll_list_temp_gomp_all_data_subset.RDS") }
  cat(cat(as.character(Sys.time()), file=logFile, append=TRUE, sep = "\n"), file=logFile, append=TRUE, sep = "\n")
}


ll_list_temp = list() 
for (i in 1:length(d1)) {
  ll_list_temp_chunk = mclapply(d1[[i]],function (x) do.call(vB_TMB_parall_validation_choice_rand_choice_cov.f,data_region_list_full[[x]]), 
                                mc.cores = 8, mc.preschedule = F)
  ll_list_temp = c(ll_list_temp,ll_list_temp_chunk)
  rm(ll_list_temp_chunk)
  if (validation == 1) {
    saveRDS(ll_list_temp,"ll_list_temp_vb_subset.RDS") }
  if (validation == 0) {
    saveRDS(ll_list_temp,"ll_list_temp_vb_all_data_subset.RDS") }
  
  cat(cat(as.character(Sys.time()), file=logFile, append=TRUE, sep = "\n"), file=logFile, append=TRUE, sep = "\n")
}


sequential = 0 # if sequential == 1, models are fitted sequentially and not in parallel (useful if you don't have access to muliple cores)

if (sequential == 1) {

ll_list_temp = list()
# 
 for (i in 1:length(data_region_list_full)) {
  
  ll_list_temp[[i]] = gomp_vB_TMB_parall_validation_choice_rand_choice_cov.f(data_region_df = data_region_list_full[[i]]$data_region_df,
                                                                               mark_all_pop_df = data_region_list_full[[i]]$mark_all_pop_df,
                                                                               age_cut = age_cut,cont = data_region_list_full[[i]]$cont,
                                                                               data_compl = data_region_list_full[[i]]$data_compl,
                                                                               rand_eff_n = data_region_list_full[[i]]$rand_eff_n,
                                                                               linf_var = data_region_list_full[[i]]$linf_var,
                                                                               k_var = data_region_list_full[[i]]$k_var,
                                                                               t0_var = data_region_list_full[[i]]$t0_var,
                                                                               mod_id = data_region_list_full[[i]]$mod_id,
                                                                               valid = data_region_list_full[[i]]$valid,
                                                                               seed = data_region_list_full[[i]]$seed)
  if (i == length(data_region_list_full)){
    saveRDS(ll_list_temp,"ll_list_temp_gomp_subset.RDS")
  }
 
 }
 
 for (i in 1:length(data_region_list_full)) {
   
   ll_list_temp[[i]] = vB_TMB_parall_validation_choice_rand_choice_cov.f(data_region_df = data_region_list_full[[i]]$data_region_df,
                                                                              mark_all_pop_df = data_region_list_full[[i]]$mark_all_pop_df,
                                                                              age_cut = age_cut,cont = data_region_list_full[[i]]$cont,
                                                                              data_compl = data_region_list_full[[i]]$data_compl,
                                                                              rand_eff_n = data_region_list_full[[i]]$rand_eff_n,
                                                                              linf_var = data_region_list_full[[i]]$linf_var,
                                                                              k_var = data_region_list_full[[i]]$k_var,
                                                                              t0_var = data_region_list_full[[i]]$t0_var,
                                                                              mod_id = data_region_list_full[[i]]$mod_id,
                                                                              valid = data_region_list_full[[i]]$valid,
                                                                              seed = data_region_list_full[[i]]$seed)
   if (i == length(data_region_list_full)){
     saveRDS(ll_list_temp,"ll_list_temp_vb_subset.RDS")
   }
   
 }

}
 
 

## read the RDS of the gomp and vb results in a list
ll_list_temp = c(readRDS("ll_list_temp_vb_subset.RDS"), readRDS("ll_list_temp_gomp_subset.RDS"))


## Save data frame of predictions pred_df (both hindcasting and validation), of perforamnce metrics rsq_df, of validation test_df, and training data train_df (when there is no validation, i.e. all data is used for training) 

pred_df = data.frame()
rsq_df = data.frame()
test_df = data.frame()
train_df = data.frame()


for (i in 1:length(ll_list_temp)) {
  
  pred_df = bind_rows(pred_df, ll_list_temp[[i]]$pred_df %>% filter(., Age == 1))  # For predictions I just save Age = 1, otherwise the dataset becomes un-manageable
  rsq_df = bind_rows(rsq_df,ll_list_temp[[i]]$model_rsq)
  test_df = bind_rows(test_df,ll_list_temp[[i]]$test_df)
  train_df = bind_rows(train_df,ll_list_temp[[i]]$train_df)  
  print(i)
  
}


### extract single model prediction with prediction and parameter confidence intervals for both vBGF and GGF



pred_model = "mod_3_rand_l_Pop_k_Pop_t0_Pop"  ## the model has to be among the models that you fitted
pred_traj_df = data.frame()
test_traj_df = data.frame()

for (i in 1:length(ll_list_temp)) {

  if(!is.na(ll_list_temp[[i]]$pred_df$model[1])) {
    if (ll_list_temp[[i]]$pred_df$model[1] ==  pred_model) {
      pred_traj_df = bind_rows(pred_traj_df, ll_list_temp[[i]]$pred_df %>% filter(., Age <=15))
      if (ll_list_temp[[i]]$pred_df$func[1] == "vb") {
        ll_list_temp[[i]]$test_df$func = "vb"
        test_traj_df = bind_rows(test_traj_df, ll_list_temp[[i]]$test_df)
      }
      if (ll_list_temp[[i]]$pred_df$func[1] == "gomp") {
        ll_list_temp[[i]]$test_df$func = "gomp"
        test_traj_df = bind_rows(test_traj_df, ll_list_temp[[i]]$test_df)
      }
    }
  }
  print(i)

}

pred_traj_df = filter(pred_traj_df, cont == min(cont))


####


#### Add population and species columns

pred_df$Species = NA

pred_df$Species[which(pred_df$Pop == "LIdri_MT")] = "MT"
pred_df$Species[which(pred_df$Pop == "UIdri_MT")] = "MT"
pred_df$Species[which(pred_df$Pop == "LIdri_RT")] = "RT"
pred_df$Species[which(pred_df$Pop == "UVol_BT")] = "BT"

if (pred_df$Pop[i] == "LIdri_MT") {pred_df$Species[i] = "MT"}
if (pred_df$Pop[i] == "UIdri_MT") {pred_df$Species[i] = "MT"}
if (pred_df$Pop[i] == "LIdri_RT") {pred_df$Species[i] = "RT"}
if (pred_df$Pop[i] == "UVol_BT") {pred_df$Species[i] = "BT"}


#### summarize model summaries. Only models that converged for all test replicates are included 

avg_rsq_df = rsq_df %>% group_by(model, func) %>% 
  summarise(n = n(),
            rsq_gam_train = mean(rsq_gam_train),
            rsq_gam_test_mean = mean(rsq_gam_test),
            rsq_gam_test_sd = sd(rsq_gam_test),
            logRMSE_gam_test = mean(logRMSE_gam_test),
            logRMSE_gam_train = mean(logRMSE_gam_train),
            RMSE_gam_test = mean(RMSE_gam_test),
            RMSE_gam_train = mean(RMSE_gam_train),
            mean_error_gam_train = mean(mean_error_gam_train),
            max_error_gam_test = mean(max_error_gam_test),
            id_gam = mean(id_gam),
            age_gam = mean(age_gam),
            std = mean(std),
            conv = mean(conv)) %>% 
  filter(., std == 1) %>% 
  arrange(., desc(rsq_gam_test_mean))


# if no validation, I also add a column with the AIC of model on the full data set

if (validation == 0) {
  
  avg_rsq_df = avg_rsq_df %>% inner_join(., select(rsq_df, model, func, AIC))
  #saveRDS(avg_rsq_df,"avg_rsq_df_all.RDS")
}




#####
#####
#####
##### Plots and stats analyses. Plots are saved sequentally on list plot_list
#####
#####
#####

plot_list = list()

size.title = 15
line.lwd = 0.35
lwd.real = 1
size.label.x = 18
size.text.x = 14
size.point = 2
size.label.y = 18
size.text.y = 14
size.legend.text = 15
size.legend.title = 20
unit.legend.h = 1.8
unit.legend.w = 1.8
size.ann = 10
colour.axis = "gray20"
colour.theme = "black"
colour.axis.line = "gray20"
colour.line = "gray50"
label.T = "Heterozygosity"
max_size_dot = 8
alpha.min = 0.35

## Theme to be used for all plots

theme.out =  theme(plot.title = element_text(lineheight=.8, face="bold", size = size.title,hjust = 0.5), 
                   plot.background = element_blank()
                   ,panel.grid.major = element_blank()
                   ,panel.grid.minor = element_blank()
                   ,panel.border = element_blank()
                   ,panel.background = element_blank(),
                   axis.line = element_line(color = 'black'),
                   plot.margin = unit(c(0.5,1.2,0.5,1.2), "cm"),
                   axis.title.x = element_text(size=size.label.x,vjust=-2),
                   axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
                   axis.title.y = element_text(size=size.label.x, vjust = 4),
                   axis.text.y  = element_text(size=size.text.x),
                   legend.title = element_blank(),
                   legend.text = element_text(size = size.legend.text),
                   legend.position = c(0.9, 0.9),
                   legend.key = element_rect(fill = "white")) 



#### Plot of distribution of Linf, A, k, kG, correlation betwwen them, and density plot. Targ is the model used


targ = "mod_3_rand_l_Pop_k_Pop_t0_Pop" # this is one of the model in the example. If another model is used, this has to be changed accordingly 

func_targ = c("gomp", "vb")

size.legend.text = 15
#max_linf_x = 850
max_linf_x = 470

plot_list = list()

for (i in 1:length(func_targ)) {
  
  plot_list[[i]] = as.list(data.frame(func = as.character(func_targ[i]), sum_linf_df = NA,
                                      plot_l_inf = NA,
                                      plot_k = NA, plot_corr = NA, plot_dens = NA))
  
  plot_list[[i]]$func = as.character(plot_list[[i]]$func)
  
  min_cont = min(unique(pred_df$cont[which(pred_df$model==targ & 
                                             pred_df$func == func_targ[i])]))
  
  targ_mod = filter(pred_df, model == targ, func == func_targ[i],
                    cont == min_cont)
  
  
  plot_list[[i]]$sum_linf_df = targ_mod %>% group_by(Mark) %>%
    summarise(n = n(), linf = mean(linf),
              k = mean(k), Pop = unique(Pop), Species = unique(Species), func = func)
  
  
  
  
  if (plot_list[[i]]$sum_linf_df$func[1] == "gomp"){
    plot_list[[i]]$plot_l_inf = ggplot(data = plot_list[[i]]$sum_linf_df, aes(x = linf, col = Species, 
                                                                              shape = Pop, lty = Pop)) +
      geom_density(alpha = 0.1, lwd = 1) + theme_minimal() + scale_color_manual(values = c("gray30","gray50", "gray70")) + scale_linetype_manual(
        values = c("solid","longdash","dotted","12345678")) +
      labs(x = "A (mm)") +
      labs(y = "Density") +
      scale_x_continuous(limits = c(150,max_linf_x)) +
      scale_y_continuous(limits = c(0,0.04)) +
      theme(
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.5,1.2,0.5,1.2), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2),
        axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
        axis.title.y = element_text(size=size.label.x, vjust = 4),
        axis.text.y  = element_text(size=size.text.x),
        legend.text = element_text(size = size.legend.text),
        legend.title = element_blank()
      )
  }
  
  if (plot_list[[i]]$sum_linf_df$func[1] == "vb"){
    plot_list[[i]]$plot_l_inf = ggplot(data = plot_list[[i]]$sum_linf_df, aes(x = linf, col = Species, 
                                                                              shape = Pop, lty = Pop)) +
      geom_density(alpha = 0.1, lwd = 1) + theme_minimal() + scale_color_manual(values = c("gray30","gray50", "gray70")) + scale_linetype_manual(
        values = c("solid","longdash","dotted","12345678")) +
      labs(x = bquote(L[infinity](mm))) +
      labs(y = "Density") +
      scale_x_continuous(limits = c(150,max_linf_x)) +
      scale_y_continuous(limits = c(0,0.04)) +
      theme(
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.5,1.2,0.5,1.2), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2),
        axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
        axis.title.y = element_text(size=size.label.x, vjust = 4),
        axis.text.y  = element_text(size=size.text.x),
        legend.text = element_text(size = size.legend.text),
        legend.title = element_blank()
      )
  }
  
  
  if (plot_list[[i]]$sum_linf_df$func[1] == "gomp"){
    plot_list[[i]]$plot_k = ggplot(data = plot_list[[i]]$sum_linf_df, aes(x = k, col = Species, shape = Pop, lty = Pop)) +
      geom_density(alpha = 0.1) + theme_minimal() + scale_color_manual(values = c("gray30","gray50", "gray70")) + 
      scale_linetype_manual(
        values = c("solid","longdash","dotted","12345678")) +
      labs(y = bquote(k[G] (y^-1))) +
      labs(y = "Density") +
      scale_x_continuous(limits = c(0,1)) +
      theme(
        axis.line = element_line(colour = "black")
      )
  }
  
  
  if (plot_list[[i]]$sum_linf_df$func[1] == "vb"){
    plot_list[[i]]$plot_k = ggplot(data = plot_list[[i]]$sum_linf_df, aes(x = k, col = Species, shape = Pop, lty = Pop)) +
      geom_density(alpha = 0.1) + theme_minimal() + scale_color_manual(values = c("gray30","gray50", "gray70")) + 
      scale_linetype_manual(
        values = c("solid","longdash","dotted","12345678")) +
      labs(y = bquote(k(y^-1))) +
      labs(y = "Density") +
      scale_x_continuous(limits = c(0,1)) +
      theme(
        axis.line = element_line(colour = "black")
      )
  }
  
  
  
  if (plot_list[[i]]$sum_linf_df$func[1] == "vb"){
    
    plot_list[[i]]$plot_corr = ggplot(data = plot_list[[i]]$sum_linf_df, aes(x = linf, y = k, 
                                                                             fill = Pop, shape = Pop)) +
      geom_point() + theme_minimal() + scale_color_manual(values = c("gray30","gray50", "gray70")) + scale_shape_manual(
        values = c(1,2,3,4)) +
      labs(x = bquote(L[infinity](mm))) +
      labs(y = bquote(k(y^-1))) +
      scale_x_continuous(limits = c(150,max_linf_x)) +
      scale_y_continuous(limits = c(0.1,0.7),breaks = c(0.2,0.4,0.6)) +
      theme(
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.5,1.2,0.5,1.2), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2),
        axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
        axis.title.y = element_text(size=size.label.x, vjust = 4),
        axis.text.y  = element_text(size=size.text.x),
        legend.text = element_text(size = size.legend.text),
        legend.title = element_blank()
      )
  } 
  
  if (plot_list[[i]]$sum_linf_df$func[1] == "gomp"){
    
    plot_list[[i]]$plot_corr = ggplot(data = plot_list[[i]]$sum_linf_df, aes(x = linf, y = k, 
                                                                             fill = Pop, shape = Pop)) +
      geom_point() + theme_minimal() + scale_color_manual(values = c("gray30","gray50", "gray70")) + scale_shape_manual(
        values = c(1,2,3,4)) +
      labs(x = "A (mm)") +
      labs(y = bquote(k[G] (y^-1))) +
      scale_x_continuous(limits = c(150,max_linf_x)) +
      scale_y_continuous(limits = c(0.1,0.9), breaks = c(0.3,0.6,0.9)) +
      theme(
        axis.line = element_line(colour = "black"),plot.margin = unit(c(0.5,1.2,0.5,1.2), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2),
        axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
        axis.title.y = element_text(size=size.label.x, vjust = 4),
        axis.text.y  = element_text(size=size.text.x),
        legend.text = element_text(size = size.legend.text),
        legend.title = element_blank()
      )
  } 
  
  
  if (plot_list[[i]]$sum_linf_df$func[1] == "gomp"){
    
    plot_list[[i]]$plot_dens = ggplot(data = plot_list[[i]]$sum_linf_df, aes(x = linf, y = k, col = Pop, 
                                                                             fill = Pop, label = Pop)) +
      theme_minimal() + scale_color_manual(values = c("gray30","gray50", "gray70","gray80")) +
      labs(x = "A (mm)") +
      labs(y = bquote(k[G] (y^-1))) +
      scale_x_continuous(limits = c(150,max_linf_x)) +
      scale_y_continuous(limits = c(0.1,0.9)) +
      geom_density_2d(lwd = 0.4) +
      theme(
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.5,1.2,0.5,1.2), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2),
        axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
        axis.title.y = element_text(size=size.label.x, vjust = 4),
        axis.text.y  = element_text(size=size.text.x),
        legend.text = element_text(size = size.legend.text),
        legend.title = element_blank()
      )
  }
  
  
  if (plot_list[[i]]$sum_linf_df$func[1] == "vb"){
    
    plot_list[[i]]$plot_dens = ggplot(data = plot_list[[i]]$sum_linf_df, aes(x = linf, y = k, col = Pop, 
                                                                             fill = Pop, label = Pop)) +
      theme_minimal() + scale_color_manual(values = c("gray30","gray50", "gray70","gray80")) +
      labs(x = bquote(L[infinity](mm))) +
      labs(y = bquote(k(y^-1))) +
      scale_x_continuous(limits = c(150,max_linf_x)) +
      scale_y_continuous(limits = c(0.1,0.9)) +
      geom_density_2d(lwd = 0.4) +
      theme(
        axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.5,1.2,0.5,1.2), "cm"),
        axis.title.x = element_text(size=size.label.x,vjust=-2),
        axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
        axis.title.y = element_text(size=size.label.x, vjust = 4),
        axis.text.y  = element_text(size=size.text.x),
        legend.text = element_text(size = size.legend.text),
        legend.title = element_blank()
      )
  }
  
}


### Plot of distribution of L_inf and A
### 
Plot_linf_all = plot_grid(plot_list[[1]]$plot_l_inf,
                          plot_list[[2]]$plot_l_inf,
                          labels = c(" GGF", "vBGF"),
                          nrow = 2, align = "h",hjust = -3)

plot_list[[1]] = plot(Plot_linf_all)

### Plot of distribution of k and kg
### 
Plot_k_all = plot_grid(plot_list[[1]]$plot_k,
                       plot_list[[2]]$plot_k,
                       labels = c("GGF", "vBGF"),
                       nrow = 2, align = "v",hjust = -6)

plot_list[[2]] = plot(Plot_k_all)

### Plot of correlation of L_inf and k and A and kg
### 

Plot_corr = plot_grid(plot_list[[1]]$plot_corr,
                      plot_list[[2]]$plot_corr,
                      labels = c(" GGF", "vBGF"),
                      nrow = 2, align = "h", hjust = -3)

plot_list[[3]] = plot(Plot_corr)

#####
#####
#####





#### Statistics for each model/population on model parameters

#### Limit digits in avg_rsq_d numeric variables

is.num <- sapply(avg_rsq_df, is.numeric)
avg_rsq_df[is.num] <- lapply(avg_rsq_df[is.num], round, 2)

model_df = unique(avg_rsq_df$model)

## param_corr_df has Pearson's correlation estimate (and p value) for L_inf-k or A-kg for Populations/models combinations  

param_corr_df = data.frame(model = rep(NA, 8 * length(model_df)),
                           Pop = NA,
                           func = NA,
                           corr_est = NA,
                           corr_p = NA,
                           Linf_mean = NA,
                           Linf_sd = NA,
                           k_mean = NA,
                           k_sd = NA)

pop_df = c("LIdri_MT", "UIdri_MT", "UVol_BT", "LIdri_RT")

func_df = c("vb", "gomp")

cont = 1



for (i in 1:length(model_df)) {
  
  cont_min = min(unique(filter(pred_df, model == model_df[i])$cont)) # there are multiple replicates for the same model (the seeds), I take the first replicate
  
  for (z in 1:length(func_df)) {
    
    for (j in 1:length(pop_df)) {
      
      prov_df = filter(pred_df, model == model_df[i], func == func_df[z], Pop == pop_df[j], !is.na(linf))
      
      #with(test, cor.test(linf,k))
      
      param_corr_df$model[cont] = model_df[i]
      param_corr_df$Pop[cont] =  pop_df[j]
      param_corr_df$func[cont] = func_df[z]
      param_corr_df$corr_est[cont] = cor.test(prov_df$linf, prov_df$k)$estimate
      param_corr_df$corr_p[cont] = cor.test(prov_df$linf, prov_df$k)$p.value
      param_corr_df$Linf_mean[cont] = mean(prov_df$linf, na.rm = T)
      param_corr_df$Linf_sd[cont] = sd(prov_df$linf, na.rm = T)
      param_corr_df$k_mean[cont] = mean(prov_df$k, na.rm = T)
      param_corr_df$k_sd[cont] = sd(prov_df$k, na.rm = T)
      
      cont = cont + 1
      
    }
  }
  
}



#### Model for correlation test of model estimates of asymptotic size within populations and plot of correlation


## For each model and population, mean estimate and standard erros of asymptotic size for vBGF and GGF models
## 
test_param_corr = select(filter(param_corr_df, func == "vb"), model, Pop, func, Linf_mean, Linf_sd) %>%
  left_join(., select(filter(param_corr_df, func == "gomp"), model, Pop, func, Linf_mean, Linf_sd), by = c("model", "Pop")) %>% 
  rename(., Linf_vb = Linf_mean.x, Linf_gomp = Linf_mean.y,
         Linf_vb_sd = Linf_sd.x, Linf_gomp_sd = Linf_sd.y)

### Plot of asymptotic size (point estimate and 95% CIs) estimated with Gompertz and von Bertalanffy models

Plot_corr_linf = ggplot(data = test_param_corr, aes(x = Linf_gomp, y = Linf_vb, shape = Pop, col = Pop)) +
  geom_point(size = 3, stroke = 1.5) + 
  geom_errorbar(aes(ymin=Linf_vb-2*Linf_vb_sd, ymax=Linf_vb+2*Linf_vb_sd), lwd = 0.2,
                show.legend = F) +
  geom_errorbarh(aes(xmin=Linf_gomp-2*Linf_gomp_sd, xmax=Linf_gomp+2*Linf_gomp_sd), lwd = 0.2,
                 show.legend = F) +
  scale_color_manual(values = c("gray30","gray50", "gray60","gray70")) +
  scale_shape_manual(values = c(1,2,3,4)) + 
  theme.out +
  labs(y = bquote(L[infinity](mm))) +
  labs(x = "A (mm)") +
  geom_abline(slope = 1, intercept = 0,
              na.rm = FALSE, show.legend = NA, lty = 2) +
  scale_x_continuous(limits = c(165,850), breaks = c(200,300, 400, 500, 600, 700, 800)) +
  scale_y_continuous(limits = c(150,500),breaks = c(200,300,400, 500)) +
  theme(
    axis.line = element_line(colour = "black"),
    plot.margin = unit(c(0.5,1.2,0.5,1.2), "cm"),
    axis.title.x = element_text(size=size.label.x,vjust=-2),
    axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
    axis.title.y = element_text(size=size.label.x, vjust = 4),
    axis.text.y  = element_text(size=size.text.x),
    legend.text = element_text(size = size.legend.text),
    legend.title = element_blank()
  ) +
  coord_fixed(ratio = 1)

plot_list[[4]] = plot(Plot_corr_linf)

#####
#####
#####

#### Find the worst predicted individuals and see how it is predicted by the one of the vBGF and GGF model

## Worst predicted individual (n is the number of models in which that individual was the worst predicted)
worst_pred = rsq_df %>% 
  right_join(., select(avg_rsq_df, model, func)) %>% 
  # group_by(id_gam, age_gam, func) %>% 
  group_by(id_gam, age_gam) %>% 
  summarise(n = n()) %>% 
  arrange(., desc(n)) %>% 
  rename(Mark_ind = id_gam) %>% 
  left_join(., select(mark_all_pop_df,Mark_ind, Pop)) 

worst_pred = worst_pred[1,] # just the worst fish predicted, for some reason top_n does not work

time_data = filter(all_pop_df, Mark %in% worst_pred$Mark_ind) %>%  group_by(Mark) %>% 
  summarise(Age = max(Age,na.rm = T)) %>% left_join(.,select(all_pop_df, Mark,Age, Length, Pop))

worst_pred = worst_pred %>% left_join(.,select(time_data, Mark,Length), by = c("Mark_ind" = "Mark")) %>% rename(Mark = Mark_ind)

#### Plot of worst predicted individuals with in the background the trajectories of the individuals in their populations

max_age = 7.5 

Plot_wrong_pred = ggplot(data = filter(all_pop_df, Mark %in% worst_pred$Mark), aes(x = Age, y = Length, group = Mark, label = Pop, lty = Pop)) + 
  geom_line(lwd = 1) +
  scale_linetype_manual(values = c(2,1)) +
  guides(lty = F) +
  theme.out + 
  geom_text_repel(data = time_data, aes(x = Age, y = Length, label = Pop))  +
  geom_point(data = worst_pred, aes(x = age_gam, y = Length), size = 2) +
  scale_y_continuous(limits = c(0,400)) +
  scale_x_continuous(limits = c(0,max_age), breaks = seq(1,max_age,1)) +
  geom_line(data = filter(all_pop_df, Pop == "UVol_BT", Age <=7), aes(x = Age, y = Length, group = Mark), lwd = 0.03)  +
  geom_line(data = filter(all_pop_df, Pop == "UIdri_MT", Age <=7, Mark!=3439), aes(x = Age, y = Length, group = Mark), lwd = 0.07, lty =2) +
  labs(y = "Length (mm)") +
  labs(x = "Age (year)") 

plot_list[[5]] = plot(Plot_wrong_pred)

### Plot of empirical trajectory of the worst-predicted individual and vBGF and GGF predictions (using only the measure taken at the individual's youngest age at sampling)

pred_traj_df = data.frame()
test_traj_df = data.frame()

pred_model = "mod_3_rand_l_Pop_k_Pop_t0_Pop"  # which model to use (this has to be one of the models that were fitted)

for (i in 1:length(ll_list_temp)) {
  
  if(!is.na(ll_list_temp[[i]]$pred_df$model[1])) {
    if (ll_list_temp[[i]]$pred_df$model[1] ==  pred_model) {
      pred_traj_df = bind_rows(pred_traj_df, ll_list_temp[[i]]$pred_df %>% filter(., Age <=15))
      if (ll_list_temp[[i]]$pred_df$func[1] == "vb") {
        ll_list_temp[[i]]$test_df$func = "vb"
        test_traj_df = bind_rows(test_traj_df, ll_list_temp[[i]]$test_df)
      }
      if (ll_list_temp[[i]]$pred_df$func[1] == "gomp") {
        ll_list_temp[[i]]$test_df$func = "gomp"
        test_traj_df = bind_rows(test_traj_df, ll_list_temp[[i]]$test_df)
      }
    }
  }
  print(i)
  
}

pred_worst_df = pred_traj_df %>% 
  filter(., cont == min(pred_traj_df$cont), Mark == worst_pred$Mark)

max_age = 10.5

Plot_worst = ggplot(data = filter(pred_worst_df), aes(x = Age, y = pred_mean, group = func)) + 
  geom_line(lty = 2) +
  geom_line(data = pred_worst_df, aes(x = Age, y = Length), lwd = lwd.real) +
  theme.out + 
  scale_y_continuous(limits = c(0,350)) +
  scale_x_continuous(limits = c(0,max_age), breaks = seq(1,max_age,1)) +
  labs(x = "Age (year)", y = "Length (mm)") +
  theme(axis.title.x = element_text(size=13,vjust=-2),
        axis.text.x  = element_text(size=10, vjust = 0.5),
        axis.title.y = element_text(size=13, vjust = 4),
        axis.text.y  = element_text(size=10))



plot_list[[6]] = plot(Plot_worst)


