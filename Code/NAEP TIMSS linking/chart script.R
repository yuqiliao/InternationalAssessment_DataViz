### R script to create charts
### https://github.com/American-Institutes-for-Research/EdSurvey_internal/issues/1910
### 5/9/23 
### Yuqi Liao

### Setting
library(readr)
#library(readxl)
library(tidyverse)
library(ggtext)
library(grid)


## Read in
table6 <- read_csv(file = "Code/NAEP TIMSS linking/table-6-g8math.csv")
table8 <- read_csv(file = "Code/NAEP TIMSS linking/table-8-g8science.csv")


#make Level column a factor for coloring purposes
table6$Level <- factor(table6$Level, levels = c("Below Basic", "Basic", "Proficient", "Advanced"))
table8$Level <- factor(table8$Level, levels = c("Below Basic", "Basic", "Proficient", "Advanced"))

basic_g8math <- 469
proficient_g8math <- 556
advanced_g8math <- 637

basic_g8science <- 494
proficient_g8science <- 567
advanced_g8science <- 670

##### viz 1 #####
plotTitle <- c("<span>Achievement levels associated with the national average<br>in grade 8 2019 TIMSS mathematics</span>")
plotSubtitle <- "Education system"

#cols <- c("Below Basic" = "#dde2eb", "Basic"="#bbc7d6", "Proficient"= "#8e9fbc", "Advanced"= "#44659a") 
cols <- c("Advanced"= "#44659a", "Proficient"= "#8e9fbc",  "Basic"="#bbc7d6", "Below Basic" = "#dde2eb") 
xAxisBreaks = c(100, 200, 300, 400, 500, 600, 700, 800)
xAxisBreaksLabel = c(0, 200, 300, 400, 500, 600, 700, 1000)
xAxisBreaksMin <- sum(head(xAxisBreaks, 2))/2
xAxisBreaksMax <- sum(tail(xAxisBreaks, 2))/2

# define y axis break parallel lines
gline <- linesGrob(y = c(0, 1),x = c(-.01, .01),  gp = gpar(col = "#2d2a26", lwd = 0.66)) 

# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(#aspect.ratio = 1.2:1,
  text = element_text(size=15, family="Arial", color = "black"),
  panel.background=element_blank(),
  panel.grid = element_blank(),
  axis.title.x=element_text(size=22, margin = margin(t=15, b = 5), hjust = .5, color = "black", family = "Arial"),
  #axis.title = element_blank(),
  #axis.title.y=element_text(size=10, margin = margin(t=0, b = 5),hjust = 0,vjust = 1, angle = 0),
  axis.text.x=element_text(size=13, angle = 0, hjust = 0.5, family = "Arial"),
  #conditionally formatting the benchmarking jurisdictions
  # axis.text.y=element_text(size=15, angle = 0, hjust = 1, family = "Arial", color = "black", face = c("plain", "plain","plain","plain","plain","plain","italic","plain","plain","plain","plain","plain","italic","plain")),
  axis.text.y=element_text(size=13, angle = 0, hjust = 1, family = "Arial", color = "black"),
  axis.line.x = element_line(size = 0.5, color = "#686868"),
  axis.line.y = element_line(size = 0.5, color = "#686868"),
  axis.ticks.x = element_line(size = 0.5, "#686868"),
  axis.ticks.length =  unit(.25, "cm"),
  axis.ticks.y =  element_blank(),
  plot.title=element_markdown(size=26,family = "Arial", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
  #hjust= 0.035 to make the subtitle (y-axis label) right-aligned 
  plot.subtitle=element_text(size=18,family = "Arial", hjust= 0, vjust = 0, lineheight=1, margin = margin(t = 15, b = 30), color = "black"),
  plot.caption=element_markdown(size=14, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "Arial"),
  plot.margin = unit(c(t = .3, r = 1, b = 0.3, l = 1), "cm"),
  #legend.position ="bottom",
  legend.position = c(.92, .5),
  # legend.justification = "center",
  # legend.box.just = "left",
  #legend.title = element_blank(),
  #legend.spacing = unit(c(1), "line"),
  legend.key=element_rect(fill="white"),
  legend.key.height=unit(1.5,"line"),
  legend.key.width=unit(1.5,"line"),
  legend.text = element_text(size=15, family = "Arial", hjust= 0,lineheight=1)
)



gg <- ggplot(data = table6, mapping = (aes(y = fct_reorder(Nation, Mean), x = Mean))) +
  #proficiency level lines
  geom_segment(aes(x = basic_g8math, y = -Inf, xend = basic_g8math, yend = Inf), color = "#686868", size = 0.5, alpha=0.05, linetype = 2)+
  geom_segment(aes(x = proficient_g8math, y = -Inf, xend = proficient_g8math, yend = Inf), color = "#686868", size = 0.5, alpha=0.05, linetype = 2)+
  geom_segment(aes(x = advanced_g8math, y = -Inf, xend = advanced_g8math, yend = Inf), color = "#686868", size = 0.5, alpha=0.05, linetype = 2)+
  
  #proficiency level text
  annotate(geom="rect", xmin = basic_g8math-12, xmax = basic_g8math + 12, ymin = 24, ymax = 28, fill = "white")+
  annotate(geom="text", x = basic_g8math, y = 26, label = "Basic\n469" , color = "#404040", size = 5,  angle = 90, hjust = 0.5) +
  annotate(geom="rect", xmin = proficient_g8math-12, xmax = proficient_g8math + 12, ymin = 24, ymax = 28, fill = "white")+
  annotate(geom="text", x = proficient_g8math, y = 26, label = "Proficient\n556" , color = "#404040", size = 5,  angle = 90, hjust = 0.5) +
  annotate(geom="rect", xmin = advanced_g8math-12, xmax = advanced_g8math + 12, ymin = 24, ymax = 28, fill = "white")+
  annotate(geom="text", x = advanced_g8math, y = 26, label = "Advanced\n637" , color = "#404040", size = 5,  angle = 90, hjust = 0.5) +
  
  #mean points
  geom_segment(aes(x=100, xend=Mean, y=fct_reorder(Nation, Mean), yend=fct_reorder(Nation, Mean)), color="#b5b5b5", linetype="dotted")+
  geom_point(aes(color = Level), size = 4.5) +
  
  scale_color_manual(values = cols) + 
  scale_x_continuous(
    limits = c(0, 800),
    breaks = xAxisBreaks,
    labels = xAxisBreaksLabel,
    expand = c(0, 0)
    )+
  scale_y_discrete(
    expand = c(0,0)
  )+
  theme_white +
  coord_cartesian(xlim = c(100,800), 
                  ylim = c(0,39),
                  clip = "off") +
  
  #x-axis breaks
  #annotate(geom = "rect", xmin = 145, xmax = 155, ymin = -0.5, ymax = 0.5, fill = "red") +
  annotate(geom = "segment", x = 135, xend = 155, y = -0.3, yend = 0.3, color = "#686868", size = 0.5) +
  annotate(geom = "segment", x = 145, xend = 165, y = -0.3, yend = 0.3, color = "#686868", size = 0.5) +
  #annotate(geom = "rect", xmin = 745, xmax = 755, ymin = -0.5, ymax = 0.5, fill = "red") +
  annotate(geom = "segment", x = 735, xend = 755, y = -0.3, yend = 0.3, color = "#686868", size = 0.5) +
  annotate(geom = "segment", x = 745, xend = 765, y = -0.3, yend = 0.3, color = "#686868", size = 0.5) +
  #title
  labs(x = "Score", y = "", title = plotTitle, subtitle = plotSubtitle, 
       #caption = plotCaption
       ) 
gg

g <- ggplotGrob(gg)
g$layout$l[g$layout$name == "title"] <- 4
g$layout$l[g$layout$name == "caption"] <- 4
g$layout$l[g$layout$name == "subtitle"] <- 4

grid.draw(g)

ggsave(paste0(getwd(), "/Code/NAEP TIMSS linking/table6-v1.png"), g, width = 1200, height = 800, units = "px", scale = 3.5)



##### viz 2 #####
plotTitle <- c("<span>Achievement levels associated with the national average<br>in grade 8 2019 TIMSS science</span>")
plotSubtitle <- "Education system"

#cols <- c("Below Basic" = "#dde2eb", "Basic"="#bbc7d6", "Proficient"= "#8e9fbc", "Advanced"= "#44659a") 
cols <- c("Advanced"= "#44659a", "Proficient"= "#8e9fbc",  "Basic"="#bbc7d6", "Below Basic" = "#dde2eb") 
xAxisBreaks = c(100, 200, 300, 400, 500, 600, 700, 800)
xAxisBreaksLabel = c(0, 200, 300, 400, 500, 600, 700, 1000)
xAxisBreaksMin <- sum(head(xAxisBreaks, 2))/2
xAxisBreaksMax <- sum(tail(xAxisBreaks, 2))/2

# define y axis break parallel lines
gline <- linesGrob(y = c(0, 1),x = c(-.01, .01),  gp = gpar(col = "#2d2a26", lwd = 0.66)) 


# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(#aspect.ratio = 1.2:1,
  text = element_text(size=15, family="Arial", color = "black"),
  panel.background=element_blank(),
  panel.grid = element_blank(),
  axis.title.x=element_text(size=22, margin = margin(t=15, b = 5), hjust = .5, color = "black", family = "Arial"),
  #axis.title = element_blank(),
  #axis.title.y=element_text(size=10, margin = margin(t=0, b = 5),hjust = 0,vjust = 1, angle = 0),
  axis.text.x=element_text(size=13, angle = 0, hjust = 0.5, family = "Arial"),
  #conditionally formatting the benchmarking jurisdictions
  # axis.text.y=element_text(size=15, angle = 0, hjust = 1, family = "Arial", color = "black", face = c("plain", "plain","plain","plain","plain","plain","italic","plain","plain","plain","plain","plain","italic","plain")),
  axis.text.y=element_text(size=13, angle = 0, hjust = 1, family = "Arial", color = "black"),
  axis.line.x = element_line(size = 0.5, color = "#686868"),
  axis.line.y = element_line(size = 0.5, color = "#686868"),
  axis.ticks.x = element_line(size = 0.5, "#686868"),
  axis.ticks.length =  unit(.25, "cm"),
  axis.ticks.y =  element_blank(),
  plot.title=element_markdown(size=26,family = "Arial", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
  #hjust= 0.035 to make the subtitle (y-axis label) right-aligned 
  plot.subtitle=element_text(size=18,family = "Arial", hjust= 0, vjust = 0, lineheight=1, margin = margin(t = 15, b = 30), color = "black"),
  plot.caption=element_markdown(size=14, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "Arial"),
  plot.margin = unit(c(t = .3, r = 1, b = 0.3, l = 1), "cm"),
  #legend.position ="bottom",
  legend.position = c(.92, .5),
  # legend.justification = "center",
  # legend.box.just = "left",
  #legend.title = element_blank(),
  #legend.spacing = unit(c(1), "line"),
  legend.key=element_rect(fill="white"),
  legend.key.height=unit(1.5,"line"),
  legend.key.width=unit(1.5,"line"),
  legend.text = element_text(size=15, family = "Arial", hjust= 0,lineheight=1)
)



gg <- ggplot(data = table8, mapping = (aes(y = fct_reorder(Nation, Mean), x = Mean))) +
  #proficiency level lines
  geom_segment(aes(x = basic_g8science, y = -Inf, xend = basic_g8science, yend = Inf), color = "#686868", size = 0.5, alpha=0.05, linetype = 2)+
  geom_segment(aes(x = proficient_g8science, y = -Inf, xend = proficient_g8science, yend = Inf), color = "#686868", size = 0.5, alpha=0.05, linetype = 2)+
  geom_segment(aes(x = advanced_g8science, y = -Inf, xend = advanced_g8science, yend = Inf), color = "#686868", size = 0.5, alpha=0.05, linetype = 2)+
  
  #proficiency level text
  annotate(geom="rect", xmin = basic_g8science-12, xmax = basic_g8science + 12, ymin = 24, ymax = 28, fill = "white")+
  annotate(geom="text", x = basic_g8science, y = 26, label = "Basic\n494" , color = "#404040", size = 5,  angle = 90, hjust = 0.5) +
  annotate(geom="rect", xmin = proficient_g8science-12, xmax = proficient_g8science + 12, ymin = 24, ymax = 28, fill = "white")+
  annotate(geom="text", x = proficient_g8science, y = 26, label = "Proficient\n567" , color = "#404040", size = 5,  angle = 90, hjust = 0.5) +
  annotate(geom="rect", xmin = advanced_g8science-12, xmax = advanced_g8science + 12, ymin = 24, ymax = 28, fill = "white")+
  annotate(geom="text", x = advanced_g8science, y = 26, label = "Advanced\n670" , color = "#404040", size = 5,  angle = 90, hjust = 0.5) +
  
  #mean points
  geom_segment(aes(x=100, xend=Mean, y=fct_reorder(Nation, Mean), yend=fct_reorder(Nation, Mean)), color="#b5b5b5", linetype="dotted")+
  geom_point(aes(color = Level), size = 4.5) +
  
  scale_color_manual(values = cols) + 
  scale_x_continuous(
    limits = c(0, 800),
    breaks = xAxisBreaks,
    labels = xAxisBreaksLabel,
    expand = c(0, 0)
  )+
  scale_y_discrete(
    expand = c(0,0)
  )+
  theme_white +
  coord_cartesian(xlim = c(100,800), 
                  ylim = c(0,39),
                  clip = "off") +
  
  #x-axis breaks
  #annotate(geom = "rect", xmin = 145, xmax = 155, ymin = -0.5, ymax = 0.5, fill = "red") +
  annotate(geom = "segment", x = 135, xend = 155, y = -0.3, yend = 0.3, color = "#686868", size = 0.5) +
  annotate(geom = "segment", x = 145, xend = 165, y = -0.3, yend = 0.3, color = "#686868", size = 0.5) +
  #annotate(geom = "rect", xmin = 745, xmax = 755, ymin = -0.5, ymax = 0.5, fill = "red") +
  annotate(geom = "segment", x = 735, xend = 755, y = -0.3, yend = 0.3, color = "#686868", size = 0.5) +
  annotate(geom = "segment", x = 745, xend = 765, y = -0.3, yend = 0.3, color = "#686868", size = 0.5) +
  #title
  labs(x = "Score", y = "", title = plotTitle, subtitle = plotSubtitle, 
       #caption = plotCaption
  ) 
gg

g <- ggplotGrob(gg)
g$layout$l[g$layout$name == "title"] <- 4
g$layout$l[g$layout$name == "caption"] <- 4
g$layout$l[g$layout$name == "subtitle"] <- 4

grid.draw(g)

ggsave(paste0(getwd(), "/Code/NAEP TIMSS linking/table8-v1.png"), g, width = 1200, height = 800, units = "px", scale = 3.5)