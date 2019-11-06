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


data_region_list <- readRDS("~/Dropbox/Articoli/Limit_sampling/data_region_list.RDS")
all_pop_df <- readRDS("~/Dropbox/Articoli/Limit_sampling/all_pop_df.RDS")

length_diff_top = test = all_pop_df %>% group_by(Mark,Pop) %>% 
              mutate(Length.difference = Length - lag(Length)) %>% 
  filter(.,Length.difference < (-10))

exclude_mark = length_diff_top$Mark

all_pop_df = filter(all_pop_df, !Mark %in% exclude_mark)

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

theme.tr =  theme(plot.title = element_text(lineheight=.8, face="bold", size = size.title,hjust = 0.5), 
                  plot.background = element_blank()
                  ,panel.grid.major = element_blank()
                  ,panel.grid.minor = element_blank()
                  ,panel.border = element_blank()
                  ,panel.background = element_blank(),
                  axis.line = element_line(color = 'black'),
                  plot.margin = unit(c(0.5,1.2,0.5,1.2), "cm"),
                  axis.title.x = element_text(size=size.label.x,vjust=-2),
                  axis.text.x  = element_text(size=size.text.x, vjust = 0.5),
                  axis.title.y = element_text(size=size.label.x, vjust = 2),
                  axis.text.y  = element_text(size=size.text.x),
                  legend.title = element_blank(),
                  legend.text = element_text(size = size.legend.text),
                  legend.position = c(0.1, 0.9),
                  legend.key = element_rect(fill = "white")) 


max_age = 14

######

uppidri_tr_gg = ggplot(filter(all_pop_df, Pop == "UIdri_MT"), aes(x = Age, y = Length, group = Mark)) +
  #geom_point(aes(shape = Year), size = size.point, alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_line(lwd = line.lwd) + 
 # geom_point(data = data_growth, aes (x = Age_cor, y = Length, col = final_year, alpha = final_year), position = position_dodge(width = 0.5), size = size.point) +
  #scale_color_manual(values = c("black","gray50")) +
  #scale_alpha_manual(values = c(1,alpha.min)) +
  #scale_linetype_manual(values = c(1,1)) +
  guides(col = F) +
  guides(alpha = F) +
  guides(lty = F) +
  ggtitle("UIdri_MT") +
  
  theme.tr +
  scale_y_continuous(limits = c(100,450)) +
  scale_x_continuous(limits = c(0,max_age), breaks = seq(1,max_age,1)) +
  labs(y = "Length (mm)") +
  labs(x = "Age") 

uppidri_tr_gg


####


loidri_tr_gg = ggplot(filter(all_pop_df, Pop == "LIdri_MT"), aes(x = Age, y = Length, group = Mark)) +
  #geom_point(aes(shape = Year), size = size.point, alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_line(lwd = line.lwd) + 
  # geom_point(data = data_growth, aes (x = Age_cor, y = Length, col = final_year, alpha = final_year), position = position_dodge(width = 0.5), size = size.point) +
  #scale_color_manual(values = c("black","gray50")) +
  #scale_alpha_manual(values = c(1,alpha.min)) +
  #scale_linetype_manual(values = c(1,1)) +
  guides(col = F) +
  guides(alpha = F) +
  guides(lty = F) +
  ggtitle("LIdri_MT") +
  
  theme.tr +
  scale_y_continuous(limits = c(100,450)) +
  scale_x_continuous(limits = c(0,max_age), breaks = seq(1,max_age,1)) +
  labs(y = "Length (mm)") +
  labs(x = "Age") 

loidri_tr_gg


####

uppvol_tr_gg = ggplot(filter(all_pop_df, Pop == "UVol_BT"), aes(x = Age, y = Length, group = Mark)) +
  #geom_point(aes(shape = Year), size = size.point, alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_line(lwd = line.lwd) + 
  # geom_point(data = data_growth, aes (x = Age_cor, y = Length, col = final_year, alpha = final_year), position = position_dodge(width = 0.5), size = size.point) +
  #scale_color_manual(values = c("black","gray50")) +
  #scale_alpha_manual(values = c(1,alpha.min)) +
  #scale_linetype_manual(values = c(1,1)) +
  guides(col = F) +
  guides(alpha = F) +
  guides(lty = F) +
  ggtitle("UVol_BT") +
  
  theme.tr +
  scale_y_continuous(limits = c(100,450)) +
  scale_x_continuous(limits = c(0,max_age), breaks = seq(1,max_age,1)) +
  labs(y = "Length (mm)") +
  labs(x = "Age") 

uppvol_tr_gg

####

rtidri_tr_gg = ggplot(filter(all_pop_df, Pop == "LIdri_RT"), aes(x = Age, y = Length, group = Mark)) +
  #geom_point(aes(shape = Year), size = size.point, alpha = 0.2, position = position_dodge(width = 0.50)) +
  geom_line(lwd = line.lwd) + 
  # geom_point(data = data_growth, aes (x = Age_cor, y = Length, col = final_year, alpha = final_year), position = position_dodge(width = 0.5), size = size.point) +
  #scale_color_manual(values = c("black","gray50")) +
  #scale_alpha_manual(values = c(1,alpha.min)) +
  #scale_linetype_manual(values = c(1,1)) +
  guides(col = F) +
  guides(alpha = F) +
  guides(lty = F) +
  ggtitle("LIdri_RT") +
  
  theme.tr +
  scale_y_continuous(limits = c(100,450)) +
  scale_x_continuous(limits = c(0,max_age), breaks = seq(1,max_age,1)) +
  labs(y = "Length (mm)") +
  labs(x = "Age") 

rtidri_tr_gg


Plot_tr_gr_all = plot_grid(loidri_tr_gg,
                        uppidri_tr_gg,
                        rtidri_tr_gg,
                        uppvol_tr_gg,
                        labels = c("(a)", "(b)","(c)","(d)"),
                        nrow = 2, align = "v",hjust = -4.8)

save_plot("Plots_growth/Plot_tr_gr_all_.pdf", Plot_tr_gr_all,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.7)
