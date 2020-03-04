##### Load libraries

library(tidyverse)
library(data.table)
library(parallel)
library(brms)
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


source("load_data.r")

#### Limit digits in avg_rsq_d numeric variables

is.num <- sapply(avg_rsq_df, is.numeric)
avg_rsq_df[is.num] <- lapply(avg_rsq_df[is.num], round, 2)

#### Routine for statistics for each model/populaiton on model parameters


pred_df = readRDS("data/pred_df_all_data.RDS")

model_df = unique(avg_rsq_df$model)

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
  
  cont_min = min(unique(filter(pred_df, model == model_df[i])$cont))
  
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



test_param_corr = select(filter(param_corr_df, func == "vb"), model, Pop, func, Linf_mean, Linf_sd) %>%
  left_join(., select(filter(param_corr_df, func == "gomp"), model, Pop, func, Linf_mean, Linf_sd), by = c("model", "Pop")) %>% 
  rename(., Linf_vb = Linf_mean.x, Linf_gomp = Linf_mean.y,
            Linf_vb_sd = Linf_sd.x, Linf_gomp_sd = Linf_sd.y)


# test_param_corr = select(filter(param_corr_df, func == "vb"), model, Pop, func, Linf_mean) %>%
#   left_join(., select(filter(param_corr_df, func == "gomp"), model, Pop, func, Linf_mean), by = c("model", "Pop")) %>% 
#   rename(., Linf_vb = Linf_mean.x, Linf_gomp = Linf_mean.y)



# test_param_corr %>%  group_by(Pop) %>% 
#   summarise(cor = cor.test(Linf_vb, Linf_gomp)$estimate,
#             p_value = cor.test(Linf_vb, Linf_gomp)$p.value,
#             ratio_mean = mean(Linf_vb/Linf_gomp),
#             ratio_sd = sd(Linf_vb/Linf_gomp),
#             ratio_min = min(Linf_vb/Linf_gomp),
#             ratio_max = max(Linf_vb/Linf_gomp))

### Plot of asymptotic size estimated with Gompertz and von Bertalanffy model

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

Plot_corr_linf

ggsave("Plots_growth/Plot_corr_linf.jpg", Plot_corr_linf, width = 10, height = 10)


#### Find the worst predicted individuals


worst_pred = rsq_df %>% 
  right_join(., select(avg_rsq_df, model, func)) %>% 
  # group_by(id_gam, age_gam, func) %>% 
  group_by(id_gam, age_gam) %>% 
  summarise(n = n()) %>% 
  arrange(., desc(n)) %>% 
  rename(Mark_ind = id_gam) %>% 
  left_join(., select(mark_all_pop_df,Mark_ind, Pop)) 

worst_pred = worst_pred[1:4,] # just the first 4, for some reason top_n does not work

time_data = filter(all_pop_df, Mark %in% worst_pred$Mark_ind) %>%  group_by(Mark) %>% 
  summarise(Age = max(Age,na.rm = T)) %>% left_join(.,select(all_pop_df, Mark,Age, Length, Pop))

worst_pred = worst_pred %>% left_join(.,select(time_data, Mark,Length), by = c("Mark_ind" = "Mark")) %>% rename(Mark = Mark_ind)

#saveRDS(worst_pred,"data/worst_pred.RDS")


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
  

ggsave("Plots_growth/Plot_wrong_pred.jpg", Plot_wrong_pred, width = 9.5, height = 7.5)
#ggsave("Plots_growth/Plot_wrong_pred.pdf", Plot_wrong_pred, width = 9.5, height = 7.5)


#### Plot for prediction of test data (one good, one bad)


ll_list_temp_gomp = readRDS("data/ll_list_temp_gomp.RDS")
ll_list_temp_vb = readRDS("data/ll_list_temp_vb.RDS")
ll_list_temp = c(ll_list_temp_gomp, ll_list_temp_vb)


pred_traj_df = data.frame()
test_traj_df = data.frame()

pred_df = readRDS("data/pred_df.RDS")
pred_model = "mod_3_rand_l_Species_k_Pop_t0_Pop"  # which model to use

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


cont_rep = 39
#cont_rep = 34

pred_8145_df = pred_traj_df %>% 
  filter(., cont == cont_rep, Mark == 8145)

max_age = 10.5

lwd.real = 1

Plot_8145 = ggplot(data = filter(pred_8145_df), aes(x = Age, y = pred_mean, group = func)) + 
  geom_line(lty = 2) +
  geom_line(data = pred_8145_df, aes(x = Age, y = Length), lwd = lwd.real) +
  theme.out + 
  #geom_text_repel(data = pred_3155_df, aes(x = Age, y = Length, label = func)) +
  scale_y_continuous(limits = c(0,350)) +
  scale_x_continuous(limits = c(0,max_age), breaks = seq(1,max_age,1)) +
  labs(x = "Age (year)", y = "Length (mm)") +
  theme(axis.title.x = element_text(size=13,vjust=-2),
        axis.text.x  = element_text(size=10, vjust = 0.5),
        axis.title.y = element_text(size=13, vjust = 4),
        axis.text.y  = element_text(size=10))

pred_traj_df %>% 
  filter(., cont == 34, func == "vb") %>% 
  group_by(Mark) %>% 
  summarise(n = sum(!is.na(Length))) %>% 
  arrange(., desc(n))




pred_3155_df = pred_traj_df %>% 
  filter(., cont == cont_rep, Mark == 3155)

max_age = 10.5

Plot_3155 = ggplot(data = filter(pred_3155_df), aes(x = Age, y = pred_mean, group = func)) + 
  geom_line(lty = 2) +
  geom_line(data = pred_3155_df, aes(x = Age, y = Length), lwd = lwd.real) +
  theme.out + 
  #geom_text_repel(data = pred_3155_df, aes(x = Age, y = Length, label = func)) +
  scale_y_continuous(limits = c(0,350)) +
  scale_x_continuous(limits = c(0,max_age), breaks = seq(1,max_age,1)) +
  labs(x = "Age (year)", y = "Length (mm)") +
  theme(axis.title.x = element_text(size=13,vjust=-2),
        axis.text.x  = element_text(size=10, vjust = 0.5),
        axis.title.y = element_text(size=13, vjust = 4),
        axis.text.y  = element_text(size=10))


Plot_pred_all = plot_grid(Plot_8145,
                           Plot_3155,
                           labels = c("(a)", "(b)"),
                           nrow = 1, align = "v",hjust = -5)

Plot_pred_all

save_plot("Plots_growth/Plot_pred_all.pdf", Plot_pred_all,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.2)




#####

# ll_list_temp_gomp = readRDS("data/ll_list_temp_gomp_all_data.RDS")
# ll_list_temp_vb = readRDS("data/ll_list_temp_vb_all_data.RDS")
# ll_list_temp = c(ll_list_temp_gomp, ll_list_temp_vb)
# 
# 
# pred_traj_all_df = data.frame()
# 
# for (i in 1:length(ll_list_temp)) {
#   
#   if(!is.na(ll_list_temp[[i]]$pred_df$model[1])) {
#     if (ll_list_temp[[i]]$pred_df$model[1] ==  "mod_3_rand_l_Pop_k_Pop_t0_Pop") {
#       pred_traj_all_df = bind_rows(pred_traj_all_df, ll_list_temp[[i]]$pred_df %>% filter(., Age <=15))
#     }
#   }
#   print(i)
#   
# }
# 
# 
# 
# test_traj_df$diff = test_traj_df$Length - test_traj_df$pred_mean   
# 
# cont_min = min(unique(test_traj_df$cont))
# test = select(test_traj_df, Mark, Pop, model, func, Age, diff,cont) %>% filter(., func == "vb", cont == cont_min) %>%
#   left_join(., select(test_traj_df, Mark, Pop, model, func, Age, diff,cont) %>% filter(., func == "gomp", cont == cont_min), by = c("Mark","Age","Pop")) %>%
#   mutate(., diff_diff = diff.x - diff.y)
# 
# with(test, cor.test(diff.x,diff.y))
# with(test, plot(diff.x ~ diff.y))
# 
# cont_min = min(unique(pred_traj_df$cont))
# test_linf = pred_traj_df %>% 
#   filter(., Age == 1, cont == cont_min) %>%
#   group_by(Mark, func, Pop) %>% 
#   summarise(linf = linf, k=k, t0 = t0)
# 
# saveRDS(test_linf, "data/test_linf.RDS")
# 
# 
# filter(test_linf, Mark %in% (test %>% filter(., Pop == "LIdri_RT"))$Mark)


