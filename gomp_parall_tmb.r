## add install.packages routine

if(!require(pacman))install.packages("pacman")
pacman::p_load("tidyverse", "data.table", "parallel", "TMB", "lubridate", "devtools", "Metrics", "rlist","withr")
devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
library("TMBhelper")


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


#### Read and manipulate data

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




#### Mapping original Marks to made-up marks to avoid same Mark in different populations


mark_all_pop_df = bind_rows(loidri_mark_map,
                            uppidri_mark_map,
                            rtidri_mark_map,
                            uppvol_mark_map)


saveRDS(mark_all_pop_df, "data/mark_all_pop_df.RDS")

all_pop_df = bind_rows(loidri_df,
                       uppidri_df,
                       rtidri_df,
                       uppvol_df) %>% filter(., !is.na(Cohort), Age > 0 ,
                                             !is.na(Length), Length > 0, Month == 9)

saveRDS(all_pop_df, "data/all_pop_df.RDS")

long_all_pop_df = all_pop_df %>% group_by(Mark,Pop) %>% summarize(n = n(), min_age = min(Age)) %>% 
  rename(Mark_ind = Mark) %>%  left_join(., mark_all_pop_df)



# Find the fish with the longest time series (min 3 observations) and use them for prediction
# Predict 
# Multiple models, then they can be run in parallel with combinations of the fish to be predicted


validation = 0  # 0 is no validation, 1 is validation on test data set
data_region_list_full  = list()
seed_v = c(5,6,7,8,9)  # number of replicates for tests
#seed_v = c(5)
cont = 1

for (ss in 1:length(seed_v)) {


  
  set.seed(seed_v[ss])
  
  valid_mark = long_all_pop_df %>% filter(., n>=3) %>%  group_by(Pop) %>%
              sample_frac(.,.3) %>% rename(Mark = Mark_ind, Age = min_age)
  
  valid_mark$mark_age_kept = 1
  valid_mark$mark_valid = 1
  
  all_pop_df_loo = all_pop_df %>% left_join(., select(valid_mark, Mark, Pop, Age, mark_age_kept)) %>% 
    left_join(., select(valid_mark, Mark, Pop, mark_valid))


#### Set of models to be tested

model_v = c("Const","Species + Pop + Cohort","Species * Pop * Cohort",
"Pop","Species + Pop", "Species * Pop","Pop + Cohort",
"Pop * Cohort","Species","Cohort")

mod_comb_df = tibble(linf = model_v, k = model_v, t0 = model_v)

mod_comb_df= expand.grid(mod_comb_df) %>% filter(., t0 %in% c("Const","Pop"), k %in% c("Const","Pop"))

mod_comb_df$mod_id = 1:nrow(mod_comb_df)

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


#### Read in the files with the TMB algos and collections of results from model fitting

source("gomp_vB_TMB_parall_validation_choice_rand_choice_cov.r")
source("vB_TMB_parall_validation_choice_rand_choice_cov.r")


logFile = "log_file.txt"
cat("This is a log file for vB models", file=logFile, append=FALSE, sep = "\n")

#### Loop for parallel computations. I split the parallel computation in groups of ten and save model results along the way

max <- 10 
x <- 1:length(data_region_list_full)
d1 <- split(1:length(data_region_list_full), ceiling(x/max))


ll_list_temp = list()
for (i in 1:length(d1)) {
  ll_list_temp_chunk = mclapply(d1[[i]],function (x) do.call(gomp_vB_TMB_parall_validation_choice_rand_choice_cov.f,data_region_list_full[[x]]),
                                mc.cores = 5, mc.preschedule = F)
  ll_list_temp = c(ll_list_temp,ll_list_temp_chunk)
  rm(ll_list_temp_chunk)
  if (validation == 1) {
  saveRDS(ll_list_temp,"ll_list_temp_gomp.RDS") }
  if (validation == 0) {
    saveRDS(ll_list_temp,"ll_list_temp_gomp_all_data.RDS") }
  cat(cat(as.character(Sys.time()), file=logFile, append=TRUE, sep = "\n"), file=logFile, append=TRUE, sep = "\n")
}


ll_list_temp = list() 
for (i in 1:length(d1)) {
  ll_list_temp_chunk = mclapply(d1[[i]],function (x) do.call(vB_TMB_parall_validation_choice_rand_choice_cov.f,data_region_list_full[[x]]), 
                                mc.cores = 5, mc.preschedule = F)
  ll_list_temp = c(ll_list_temp,ll_list_temp_chunk)
  rm(ll_list_temp_chunk)
  if (validation == 1) {
  saveRDS(ll_list_temp,"ll_list_temp_vb.RDS") }
  if (validation == 0) {
  saveRDS(ll_list_temp,"ll_list_temp_vb_all_data.RDS") }
  
  cat(cat(as.character(Sys.time()), file=logFile, append=TRUE, sep = "\n"), file=logFile, append=TRUE, sep = "\n")
}


### Read data

#ll_list_temp_gomp = readRDS("ll_list_temp_gomp_single_UVol_BT.RDS")
#ll_list_temp_vb = readRDS("ll_list_temp_vb_single_UVol_BT.RDS")

#ll_list_temp_gomp = readRDS("data/ll_list_temp_gomp.RDS")
#ll_list_temp_vb = readRDS("data/ll_list_temp_vb.RDS")

#ll_list_temp_gomp = readRDS("ll_list_temp_gomp_all_data.RDS")
#ll_list_temp_vb = readRDS("ll_list_temp_vb_all_data.RDS")

ll_list_temp = c(ll_list_temp_gomp, ll_list_temp_vb)  # results are saved in lists, this binds together the lists for vb and gomp models


#### Collect in data frames predictions, summary of model performance (rsq_df), test, and train data sets

pred_df = data.frame()
rsq_df = data.frame()
test_df = data.frame()
train_df = data.frame()


for (i in 1:length(ll_list_temp)) {

  pred_df = bind_rows(pred_df, ll_list_temp[[i]]$pred_df %>% filter(., Age == 1))  # For predictions I just say Age = 1
  rsq_df = bind_rows(rsq_df,ll_list_temp[[i]]$model_rsq)
  test_df = bind_rows(test_df,ll_list_temp[[i]]$test_df)
  train_df = bind_rows(train_df,ll_list_temp[[i]]$train_df[1,])
  print(i)

}
 
# saveRDS(pred_df, "data/pred_df.RDS")

### extract single model prediction with prediction and parameter confidence intervals

pred_df = readRDS("data/pred_df.RDS")
pred_model = "mod_3_rand_l_Species_k_Pop_t0_Pop"
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


# saveRDS(avg_rsq_df, "data/avg_rsq_df.RDS")

# if no validation, I also add a column with the AIC of model on the full data set

if (validation == 0) {
  
  avg_rsq_df = avg_rsq_df %>% inner_join(., select(rsq_df, model, func, AIC))
  saveRDS(avg_rsq_df,"data/avg_rsq_df_all.RDS")
}
                                                      

