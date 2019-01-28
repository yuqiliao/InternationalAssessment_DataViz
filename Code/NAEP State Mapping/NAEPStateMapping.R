### NAEP State Mapping Figure
### 1/16/19
### Yuqi Liao, Michael Lee, & Howard Huo



### Setting things up -----
reqpkg <- c("dplyr", "ggplot2", "readxl", "grid", "grDevices", "showtext", "extrafont")

sapply(reqpkg, function(pkgi) {
  if (!pkgi %in% installed.packages()) {
    install.packages(pkgi)
  }
  library(pkgi, character.only = TRUE)
})

wd <- "G:/Data Science/GIT/InternationalAssessment_DataViz/Code/NAEP State Mapping"

setwd(wd)

### Reading in data -----
data <- read_excel("./Materials/G4Readng 2017Update_yl.xlsx") %>% 
  dplyr::select(`Testing Program`, `St`, `Upper Cut`, `Lower Cut`, `NAEP Achivement Level`) %>% 
  #getting rid of irrelevant rows (if any)
  filter(!`St` %in% c("AC", "SB", "PC", NA)) %>% 
  # if `Upper Cut` and `Lower Cut` are 0, make them NA
  mutate_at(.vars = vars(`Upper Cut`, `Lower Cut`),
            .funs = funs(ifelse(.==0, NA, .))) %>% 
  #calculate mid points
  mutate(midPoint = (`Upper Cut` + `Lower Cut`)/2 ) 


# define order of St
orderSt <- data$St
data$St <- factor(data$St, levels = orderSt)

glimpse(data)
### Plotting -----
# define yAxisBreaks
yAxisFloor <- 160
yAxisCeiling <- 270
yAxisBreaks <- seq(from = yAxisFloor, to = yAxisCeiling, by = 10)

yAxisBreaksMin <- sum(head(yAxisBreaks, 2))/2
yAxisBreaksMax <- sum(tail(yAxisBreaks, 2))/2

# define y axis break parallel lines
gline <- linesGrob(y = c(0, 1),x = c(-.01, .01),  gp = gpar(col = "#000000", lwd = 0.5)) 

# define legend position on x axis & y axis
numberSates <- length(data$St)
xAxisPoint <- round(numberSates * 9/17)
yAxisPoint <- yAxisBreaksMin
xAxisPointText <- xAxisPoint + 1
yAxisPointText <- yAxisPoint

xAxisErrorBar <- round(numberSates * 4/5)
yAxisErrorBarMin <- yAxisPoint - 2
yAxisErrorBarMax <- yAxisPoint + 2
xAxisErrorBarText <- xAxisErrorBar + 1
yAxisErrorBarText <- yAxisPoint


# define achievement level
basic <- 208
proficient <- 238


# define colors

# load font
font_add_google("Open Sans")
showtext_auto()

# define theme_general
theme_general <- theme(text=element_text(family="Open Sans", color = "#000000"),
                       panel.background=element_blank(),
                       panel.border=element_rect(color="transparent"),
                       plot.margin = unit(c(0,0,0,0), "npc"),
                       panel.grid.major.y=element_blank(),
                       panel.grid.major.x=element_blank(),
                       panel.grid.minor.x=element_blank(),
                       panel.grid.minor.y=element_blank(),
                       axis.title = element_text(family="Open Sans", color = "#000000", 
                                                 size = 10, face = "bold"),
                       axis.line.x=element_line(color="#000000", size = 0.5),
                       axis.line.y=element_blank(),
                       axis.text.x=element_blank(),
                       axis.text.y=element_text(color="#000000", size= 10),
                       axis.ticks.x=element_blank(),
                       axis.ticks.y=element_line(color="#000000", size= 0.5),
                       axis.ticks.length = unit(7.3,"points"),
                       plot.title=element_text(family="Open Sans", size= 10 ,lineheight=2, 
                                               color="#000000", face = "bold"),
                       aspect.ratio = 3.718/7.4328
                       )


# plotting
plot <- ggplot(data, aes(x = St, y = midPoint)) +
  # draw hlines and the shades between them
  scale_x_discrete() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 208, ymax = 238), fill = "#D1D3D4") +
  geom_hline(yintercept = basic, color = "#77787B") +
  geom_hline(yintercept = proficient, color = "#77787B") +
  
  # add achievement level text
  annotate("text", x = length(data$St)/2, y = basic - 5, label = "NAEP~italic(Basic)~(208)", 
           parse = TRUE, hjust = 0.5, color = "#000000", size = 3.5, family="Open Sans") +
  annotate("text", x = length(data$St)/2, y = proficient + 5, label = "NAEP~italic(Proficient)~(238)", 
           parse = TRUE, hjust = 0.5, color = "#000000", size = 3.5, family="Open Sans") +
  
  # draw error bars and point
  geom_errorbar(aes(ymin = `Lower Cut`, ymax = `Upper Cut`), size = 0.47, color = "#001871") +
  geom_text(aes(y = `Lower Cut` - 2, label = St), family="Open Sans", color = "#000000", size = 2) +
  geom_point(color = "#C69214", size = 1) +
  
  # add lines for testing program benchmarks - ask Katie to confirm
  annotate("rect", xmin = 16 - 1, xmax = 28 + 1, ymin = 220, ymax = 221 , color = "#808184", size = .5) +
  
  # add legend (fake data)
  geom_point(aes(x = xAxisPoint, y = yAxisPoint), color = "#C69214", size = 1) +
  annotate("text", x = xAxisPointText, y = yAxisPointText, family="Open Sans", 
            color = "#000000", size = 3.5, label = "NAEP equivalent score", hjust = 0) +
  geom_errorbar(aes( x = xAxisErrorBar, ymin = yAxisErrorBarMin, ymax = yAxisErrorBarMax), 
                size = 0.47, color = "#001871") +
  annotate("text", x = xAxisErrorBarText, y = yAxisErrorBarText, family="Open Sans", 
            color = "#000000", size = 3.5, label = "±2 standard errors", hjust = 0) +
  
  # add axis titles
  labs(x = "State", y = "", title = "NAEP equivalent score") +
  
  
  # apply themes 
  theme_bw() + 
  theme_general +
  
  
  # y axix and y axis break parallel lines
  scale_y_continuous(limits = c(yAxisFloor, yAxisCeiling), 
                     labels = c(0, yAxisBreaks[2:(length(yAxisBreaks) - 1)], 500), 
                     breaks = yAxisBreaks, expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  annotate("segment", x = -Inf, xend = -Inf, y = min(yAxisBreaks), yend = yAxisBreaksMin - 1, color = "#808184", size = .5) +
  annotate("segment", x = -Inf, xend = -Inf, y = yAxisBreaksMin + 1, yend = yAxisBreaksMax - 1, color = "#808184", size = .5) +
  annotate("segment", x = -Inf, xend = -Inf, y = yAxisBreaksMax + 1, yend = max(yAxisBreaks), color = "#808184", size = .5) +
  annotation_custom(gline, ymin= yAxisBreaksMin - 2, ymax= yAxisBreaksMin, xmin= -Inf, xmax= Inf) + 
  annotation_custom(gline, ymin= yAxisBreaksMin, ymax= yAxisBreaksMin + 2, xmin= -Inf, xmax= Inf) +
  annotation_custom(gline, ymin= yAxisBreaksMax - 2, ymax= yAxisBreaksMax, xmin= -Inf, xmax= Inf) + 
  annotation_custom(gline, ymin= yAxisBreaksMax, ymax= yAxisBreaksMax + 2, xmin= -Inf, xmax= Inf) 
  

# move title to the left
grid.newpage()
g <- ggplotGrob(plot)
g$layout$l[g$layout$name == "title"] <- 4
grid::grid.draw(g)



### Exporting -----

# # use ggsave() - worked in the PLS example, didn't work this time (text outline)
# library(extrafont)
# loadfonts(device = "win")
# ggsave(paste0("./Results/", "ex6", ".eps"), width = 5.61 * 2, height = 2.81 * 2, dpi = 300, family = "Open Sans")
#   
#   
# # previous code for eps output, though the text appears as image not text
# cairo_ps(paste0("./Results/", "ex7", ".eps"), width = 13.84, height = 7.86)
# grid::grid.draw(g)
# dev.off()

#postscript() - didn't work in the PLS example, but work this time

# load font again
loadfonts(device = "postscript")
# save as
setEPS()
postscript(paste0("./Results/", "ex3", ".eps"), family = "Open Sans", width = 7.4328, height = 3.718) #width and height are in inches
grid::grid.draw(g)
dev.off()


