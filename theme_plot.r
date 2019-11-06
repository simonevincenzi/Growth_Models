size.title = 15
line.lwd = 1
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
# leg.x = 0.15
# leg.y = 0.15

## Theme to be used for all plots

theme.pop =  theme(plot.title = element_text(lineheight=.8, face="bold", size = size.title,hjust = 0.5), 
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
                   legend.spacing.y = unit(5,"cm"),
                   legend.position = c(leg.x, leg.y),
                   legend.key = element_rect(fill = "white", size = 5),
                   legend.key.size = unit(1.5,"lines")) 
