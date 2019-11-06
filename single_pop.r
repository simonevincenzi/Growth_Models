
single_rds = c("ll_list_temp_vb_single_UVol_BT","ll_list_temp_gomp_single_UVol_BT",
  "ll_list_temp_vb_single_UIdri_MT","ll_list_temp_gomp_single_UIdri_MT",
  "ll_list_temp_vb_single_LIdri_RT","ll_list_temp_gomp_single_LIdri_RT",
  "ll_list_temp_vb_single_LIdri_MT", "ll_list_temp_gomp_single_LIdri_MT")


pred_single_df = data.frame()
rsq_single_df = data.frame()
test_single_df = data.frame()
train_single_df = data.frame()


for (j in 1:length(single_rds)) {

print(j)
  
ll_list_temp = readRDS(paste(single_rds[j],".RDS",sep = ""))

pred_df = data.frame()
rsq_df = data.frame()
test_df = data.frame()
train_df = data.frame()


for (i in 1:length(ll_list_temp)) {
  
  if (length(ll_list_temp[[i]])>1) {
  
  pred_df = bind_rows(pred_df, ll_list_temp[[i]]$pred_df %>% filter(., Age == 1))
  rsq_df = bind_rows(rsq_df,ll_list_temp[[i]]$model_rsq)
  #test_df = bind_rows(test_df,ll_list_temp[[i]]$test_df)
  #train_df = bind_rows(train_df,ll_list_temp[[i]]$train_df[1,])
  print(i)
  
}

pred_df$Species = NA
#$for (i in 1:nrow(pred_df)) {

pred_df$Species[which(pred_df$Pop == "LIdri_MT")] = "MT"
pred_df$Species[which(pred_df$Pop == "UIdri_MT")] = "MT"
pred_df$Species[which(pred_df$Pop == "LIdri_RT")] = "RT"
pred_df$Species[which(pred_df$Pop == "UVol_BT")] = "BT"

if (pred_df$Pop[i] == "LIdri_MT") {pred_df$Species[i] = "MT"}
if (pred_df$Pop[i] == "UIdri_MT") {pred_df$Species[i] = "MT"}
if (pred_df$Pop[i] == "LIdri_RT") {pred_df$Species[i] = "RT"}
if (pred_df$Pop[i] == "UVol_BT") {pred_df$Species[i] = "BT"}
#}

avg_rsq_single_df = rsq_df %>% group_by(model, func) %>% 
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

avg_rsq_single_df$Pop = pred_df$Pop[1]
avg_rsq_single_df$Species = pred_df$Species[1]

#test_df$Pop = pred_df$Pop[1]
#test_df$Species = pred_df$Species[1]

#train_df$Pop = pred_df$Pop[1]
#train_df$Species = pred_df$Species[1]

pred_single_df = bind_rows(pred_single_df, pred_df)
rsq_single_df = bind_rows(rsq_single_df, rsq_df)
#test_single_df = bind_rows(test_single_df, test_df)
#train_single_df = bind_rows(train_single_df, train_df)

}

}

