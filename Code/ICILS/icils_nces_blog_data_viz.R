### ICILS 2018 data viz for NCES blog
### 11/7/22 
### Yuqi Liao

### Setting
library(readr)
library(readxl)
library(tidyverse)
library(ggtext)
library(grid)

### viz 1 #####

## Read in
cil_data <- read_xlsx(path = paste0(getwd(), "/Code/ICILS/icils data/icils2018_cil.xlsx"), sheet = 1)
cil_footnote <- read_xlsx(path = paste0(getwd(), "/Code/ICILS/icils data/icils2018_cil.xlsx"), sheet = 2)
cil_avg <- read_xlsx(path = paste0(getwd(), "/Code/ICILS/icils data/icils2018_cil.xlsx"), sheet = 3) %>% pull()
cil_avg <- cil_avg - 200

# viz
plotTitle <- c("Average CIL scores of 8th-grade students, by education system: 2018")
plotSubtitle <- "Education system"
# plotCaption <- "<span>! Interpret data with caution. The coefficient of variation (CV) for this estimate is between 30 and 50 percent.<br>
# NOTE: The status dropout rate is the percentage of 16- to 24-year-olds who are not enrolled in high school and who lack a high school credential (either a<br>diploma or an alternative credential such as a GED certificate). Data are based on sample surveys of the civilian noninstitutionalized population, which<br>excludes persons in the military and persons living in institutions (e.g., prisons or nursing facilities). Pacific Islander student group is not shown as<br>reporting standards were not met.<br>
# SOURCE: U.S. Department of Commerce, Census Bureau, Current Population Survey (CPS), October, 2010 and 2020. See <i style='font-family: PublicoText-Italic'>Digest of Education Statistics 2021,</i> <br>table 219.73.</span>"
cols <- c("#489FDF", "#071D49")
xAxisBreaks = c(0, 100, 200, 300, 400, 500)
xAxisBreaksLabel = c(0, 300, 400, 500, 600, 700)
xAxisBreaksMin <- sum(head(xAxisBreaks, 2))/2
xAxisBreaksMax <- sum(tail(xAxisBreaks, 2))/2

# define y axis break parallel lines
gline <- linesGrob(y = c(0, 1),x = c(-.01, .01),  gp = gpar(col = "#2d2a26", lwd = 0.66)) 


# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(#aspect.ratio = 1.2:1,
  text = element_text(size=20, family="Arial", color = "black"),
  panel.background=element_blank(),
  panel.grid = element_blank(),
  #axis.title.x=element_text(size=24, margin = margin(t=15, b = 5), hjust = .5, color = "black", family = "PublicoText-Bold"),
  axis.title = element_blank(),
  #axis.title.y=element_text(size=10, margin = margin(t=0, b = 5),hjust = 0,vjust = 1, angle = 0),
  axis.text.x=element_text(size=18, angle = 0, hjust = 0.5, family = "Arial"),
  axis.text.y=element_markdown(size=18, hjust = 1, family = "Arial", color = "black"),
  axis.line.x = element_line(size = 1, color = "#686868"),
  axis.line.y = element_line(size = 1, color = "#686868"),
  axis.ticks.x = element_line(size = 1, "#686868"),
  axis.ticks.length =  unit(.25, "cm"),
  axis.ticks.y =  element_blank(),
  plot.title=element_markdown(size=26,family = "Arial", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
  #hjust= 0.035 to make the subtitle (y-axis label) right-aligned 
  plot.subtitle=element_text(size=24,family = "Arial", hjust= 0, vjust = 0, lineheight=1, margin = margin(t = 15, b = 30), color = "black"),
  plot.caption=element_markdown(size=14, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "Arial"),
  plot.margin = unit(c(t = .3, r = 1, b = 0.3, l = 1), "cm"),
  legend.position ="none",
  # legend.justification = "center",
  # legend.box.just = "left",
  legend.title = element_blank(),
  #legend.spacing = unit(c(1), "line"),
  legend.key.height=unit(1.5,"line"),
  legend.key.width=unit(1.5,"line"),
  legend.text = element_text(size=21, family = "Arialn", hjust= 0,lineheight=1)
)



gg <- ggplot(data = cil_data, mapping = (aes(y = fct_reorder(Country, ValuePlot), x = ValuePlot, fill = isUSA))) +
  #int avg line
  geom_segment(aes(x = cil_avg, y = -Inf, xend = cil_avg, yend = Inf), color = "#071d49", size = 1, alpha=0.05)+
  #bars
  geom_col(width=0.6) +
  geom_text(aes(label = Label), hjust=-0.1, size = 7)+
  scale_fill_manual(values = cols) + 
  scale_x_continuous(
    limits = c(0, 700),
    breaks = xAxisBreaks,
    labels = xAxisBreaksLabel,
    expand = c(0, 0))+
  scale_y_discrete(
    expand = c(0,0)
  )+
  theme_white +
  coord_cartesian(xlim = c(0,500), 
                  ylim = c(0,15.5),
                  clip = "off") +
  # int avg label
  annotate(geom="rect", xmin = cil_avg-15, xmax = cil_avg + 15, ymin = 14.5, ymax = 15.5, fill = "#071d49")+
  annotate(geom="text", x = cil_avg, y = 15, label = "496*" , color = "white", size = 7) +
  annotate(geom="text", x = cil_avg+20, y = 15, label = "ICILS 2018 average" , color = "black", hjust=0, size = 7) +
  #x-axis breaks
  #annotate(geom = "rect", xmin = 45, xmax = 55, ymin = -0.5, ymax = 0.5, fill = "#071d49") +
  annotate(geom = "segment", x = 35, xend = 55, y = -0.3, yend = 0.3, color = "#686868", size = 1) +
  annotate(geom = "segment", x = 45, xend = 65, y = -0.3, yend = 0.3, color = "#686868", size = 1) +
  #title
  labs(x = "", y = "", title = plotTitle, subtitle = plotSubtitle, 
       #caption = plotCaption
       ) 
gg

g <- ggplotGrob(gg)
g$layout$l[g$layout$name == "title"] <- 4
g$layout$l[g$layout$name == "caption"] <- 4
g$layout$l[g$layout$name == "subtitle"] <- 4

grid.draw(g)

ggsave(paste0(getwd(), "/Code/ICILS/icils data/icils2018_cil.png"), g, width = 1200, height = 800, units = "px", scale = 3.5)


### viz 2 #####

##read in
teaching_data <- read_xlsx(path = paste0(getwd(), "/Code/ICILS/icils data/icils2018_teaching practice.xlsx"), sheet = 1) %>% 
  pivot_longer(cols = contains("_value"), names_to = "Value_category", values_to = "Value") 
#   %>% 
#   pivot_longer(cols = contains("_se"), names_to = "SE_category", values_to = "SE")
# teaching_footnote <- read_xlsx(path = paste0(getwd(), "/Code/ICILS/icils data/icils2018_teaching practice.xlsx"), sheet = 2) 


teaching_data <- teaching_data%>% 
  separate(col = "Value_category", into = c("Category", "value"), sep = "_")

teaching_category_order <- teaching_data %>% 
  filter(Category == "ELA") %>% 
  arrange(Value) %>% 
  pull(Teaching_practice)
  

teaching_data$Teaching_practice <- factor(teaching_data$Teaching_practice, levels = teaching_category_order)

#viz

plotTitle <- c("<span>Percentage of U.S. eighth-grade teachers who often or always use ICT<br>by selected teaching practice and subject: 2018</span>")
plotSubtitle <- "Teaching practice"
# plotCaption <- "<span>! Interpret data with caution. The coefficient of variation (CV) for this estimate is between 30 and 50 percent.<br>
# NOTE: The status dropout rate is the percentage of 16- to 24-year-olds who are not enrolled in high school and who lack a high school credential (either a<br>diploma or an alternative credential such as a GED certificate). Data are based on sample surveys of the civilian noninstitutionalized population, which<br>excludes persons in the military and persons living in institutions (e.g., prisons or nursing facilities). Pacific Islander student group is not shown as<br>reporting standards were not met.<br>
# SOURCE: U.S. Department of Commerce, Census Bureau, Current Population Survey (CPS), October, 2010 and 2020. See <i style='font-family: PublicoText-Italic'>Digest of Education Statistics 2021,</i> <br>table 219.73.</span>"
cols <- c("#FBB03B", "#971B2F", "#009A44")
xAxisBreaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
xAxisBreaksLabel = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, "100%")
# xAxisBreaksMin <- sum(head(xAxisBreaks, 2))/2
# xAxisBreaksMax <- sum(tail(xAxisBreaks, 2))/2

# define y axis break parallel lines
# gline <- linesGrob(y = c(0, 1),x = c(-.01, .01),  gp = gpar(col = "#2d2a26", lwd = 0.66)) 


# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(#aspect.ratio = 1.2:1,
  text = element_text(size=20, family="Arial", color = "black"),
  panel.background=element_blank(),
  panel.grid = element_blank(),
  #axis.title.x=element_text(size=24, margin = margin(t=15, b = 5), hjust = .5, color = "black", family = "PublicoText-Bold"),
  axis.title = element_blank(),
  #axis.title.y=element_text(size=10, margin = margin(t=0, b = 5),hjust = 0,vjust = 1, angle = 0),
  axis.text.x=element_text(size=18, angle = 0, hjust = 0.5, family = "Arial"),
  axis.text.y=element_text(size=18, hjust = 1, family = "Arial", color = "black"),
  axis.line.x = element_line(size = 1, color = "#686868"),
  axis.line.y = element_line(size = 1, color = "#686868"),
  axis.ticks.x = element_line(size = 1, "#686868"),
  axis.ticks.length =  unit(.25, "cm"),
  axis.ticks.y =  element_blank(),
  plot.title=element_markdown(size=26,family = "Arial", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
  #hjust= 0.035 to make the subtitle (y-axis label) right-aligned 
  plot.subtitle=element_text(size=24,family = "Arial", hjust= 0, vjust = 0, lineheight=1, margin = margin(t = 15, b = 30), color = "black"),
  plot.caption=element_markdown(size=14, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "Arial"),
  plot.margin = unit(c(t = .3, r = 1, b = 0.3, l = 1), "cm"),
  legend.position = c(0.8, 0.5), #center-right corner
  # legend.justification = "center",
  # legend.box.just = "left",
  legend.title = element_blank(),
  #legend.spacing = unit(c(1), "line"),
  legend.key.height=unit(1.5,"line"),
  legend.key.width=unit(1.5,"line"),
  legend.text = element_text(size=21, family = "Arialn", hjust= 0,lineheight=1)
)



gg <- ggplot(data = teaching_data, mapping = (aes(x =  Value, y = Teaching_practice, fill = factor(Category)))) +
  #bars
  geom_col(width=0.8, position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = Value), hjust=-0.1,position = position_dodge2(0.8, reverse = TRUE), size = 5)+
  scale_fill_manual(values = cols) + 
  scale_x_continuous(
    limits = c(0, 100),
    breaks = xAxisBreaks,
    labels = xAxisBreaksLabel,
    expand = c(0, 0))+
  scale_y_discrete(
    #expand = c(0,0),
    labels = function(x) 
      stringr::str_wrap(x, width = 35)
  )+
  #title
  labs(x = "", y = "", title = plotTitle, subtitle = plotSubtitle, 
       #caption = plotCaption
  )+
  theme_white
   
gg

g <- ggplotGrob(gg)
g$layout$l[g$layout$name == "title"] <- 4
g$layout$l[g$layout$name == "caption"] <- 4
g$layout$l[g$layout$name == "subtitle"] <- 4

grid.draw(g)

ggsave(paste0(getwd(), "/Code/ICILS/icils data/icils2018_teaching.png"), g, width = 1200, height = 800, units = "px", scale = 3.5)
