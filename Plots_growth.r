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

#####

size.title = 15
line.lwd = 0.35
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


# ll_list_temp_gomp = readRDS("ll_list_temp_gomp.RDS")
# ll_list_temp_vb = readRDS("ll_list_temp_vb.RDS")
# ll_list_temp = c(ll_list_temp_gomp, ll_list_temp_vb)
# 
# # ll_list_temp = c(ll_list_temp_vb_all_data, ll_list_temp_gomp_all_data)
# 
# # ll_list_temp[[3]] = NULL
# 
# 
# pred_df = data.frame()
# rsq_df = data.frame()
# test_df = data.frame()
# train_df = data.frame()
# 
# 
# for (i in 1:length(ll_list_temp)) {
#   
#   pred_df = bind_rows(pred_df, ll_list_temp[[i]]$pred_df %>% filter(., Age == 1))
#   rsq_df = bind_rows(rsq_df,ll_list_temp[[i]]$model_rsq)
#   test_df = bind_rows(test_df,ll_list_temp[[i]]$test_df)
#   train_df = bind_rows(train_df,ll_list_temp[[i]]$train_df[1,])
#   print(i)
#   
# }
# 
# pred_df$Species = NA
# #$for (i in 1:nrow(pred_df)) {
# 
# pred_df$Species[which(pred_df$Pop == "LIdri_MT")] = "MT"
# pred_df$Species[which(pred_df$Pop == "UIdri_MT")] = "MT"
# pred_df$Species[which(pred_df$Pop == "LIdri_RT")] = "RT"
# pred_df$Species[which(pred_df$Pop == "UVol_BT")] = "BT"
# 
# if (pred_df$Pop[i] == "LIdri_MT") {pred_df$Species[i] = "MT"}
# if (pred_df$Pop[i] == "UIdri_MT") {pred_df$Species[i] = "MT"}
# if (pred_df$Pop[i] == "LIdri_RT") {pred_df$Species[i] = "RT"}
# if (pred_df$Pop[i] == "UVol_BT") {pred_df$Species[i] = "BT"}
# #}
# 
# saveRDS(pred_df, "pred_df.RDS")
# saveRDS(rsq_df, "rsq_df.RDS")
# saveRDS(train_df, "train_df.RDS")
# saveRDS(test_df, "test_df.RDS")
# 
# 
# pred_df = readRDS("pred_df.RDS")
# rsq_df = readRDS("rsq_df.RDS")
# train_df = readRDS("train_df.RDS")
# test_df = readRDS("test_df.RDS")
# 
# 
# 
# avg_rsq_df = rsq_df %>% group_by(model, func) %>% 
#   summarise(n = n(),
#             rsq_gam_train = mean(rsq_gam_train),
#             rsq_gam_test_mean = mean(rsq_gam_test),
#             rsq_gam_test_sd = sd(rsq_gam_test),
#             logRMSE_gam_test = mean(logRMSE_gam_test),
#             logRMSE_gam_train = mean(logRMSE_gam_train),
#             RMSE_gam_test = mean(RMSE_gam_test),
#             RMSE_gam_train = mean(RMSE_gam_train),
#             mean_error_gam_train = mean(mean_error_gam_train),
#             max_error_gam_test = mean(max_error_gam_test),
#             id_gam = mean(id_gam),
#             age_gam = mean(age_gam),
#             std = mean(std),
#             conv = mean(conv)) %>% 
#   filter(., std == 1) %>% 
#   arrange(., desc(rsq_gam_test_mean))
# 
# if (nrow(avg_rsq_df) == 24) {
#   
#   avg_rsq_df = avg_rsq_df %>% left_join(., select(rsq_df, model, func, AIC)) 
# 
# } 
# 


#### Plot of distribution of Linf, A, k, kG, correlation betwwen them, and density plot. Targ is the model used


pred_df = readRDS("data/pred_df_all_data.RDS")

targ = "mod_3_rand_l_Pop_k_Pop_t0_Pop"

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


Plot_linf_all = plot_grid(plot_list[[1]]$plot_l_inf,
                          plot_list[[2]]$plot_l_inf,
                          labels = c(" GFF", "vBGF"),
                          nrow = 2, align = "h",hjust = -3)


Plot_linf_all

Plot_k_all = plot_grid(plot_list[[1]]$plot_k,
                          plot_list[[2]]$plot_k,
                          labels = c("GFF", "vBGF"),
                          nrow = 2, align = "v",hjust = -6)


Plot_k_all


Plot_corr = plot_grid(plot_list[[1]]$plot_corr,
                          plot_list[[2]]$plot_corr,
                          labels = c(" GGF", "vBGF"),
                          nrow = 2, align = "h", hjust = -3)


Plot_corr


Plot_dens = plot_grid(plot_list[[1]]$plot_dens,
                      plot_list[[2]]$plot_dens,
                      labels = c(" GGF", "vBGF"),
                      nrow = 2, align = "h", hjust = -3)


Plot_dens


save_plot("Plots_growth/Plot_dens.pdf", Plot_dens,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 0.7,
          base_height = 10)

save_plot("Plots_growth/Plot_linf.pdf", Plot_linf_all,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 0.7,
          base_height = 10)

save_plot("Plots_growth/Plot_k.pdf", Plot_k_all,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 0.7,
          base_height = 10)

save_plot("Plots_growth/Plot_corr.pdf", Plot_corr,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 0.7,
          base_height = 10)


# save_plot("Plots_growth/Plot_corr_RT.pdf", Plot_corr,
#           ncol = 2, # we're saving a grid plot of 2 columns
#           nrow = 1, # and 2 rows
#           # each individual subplot should have an aspect ratio of 1.3
#           base_aspect_ratio = 0.7,
#           base_height = 10)





