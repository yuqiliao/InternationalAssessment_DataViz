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
data <- read_excel("./Materials/G4ReadingBottom_yl.xlsx")

# # define order of year
# orderYear <- data$year
# data$St <- factor(data$St, levels = orderSt)

glimpse(data)
### Plotting -----
barCol <- "#212C68"

# # define index of testing programs
# SBACMin <- head(which(!is.na(data$`Testing Program`) & data$`Testing Program` %in% "SBAC"),1)
# SBACMax <- tail(which(!is.na(data$`Testing Program`) & data$`Testing Program` %in% "SBAC"),1)
# ACTMin <- head(which(!is.na(data$`Testing Program`) & data$`Testing Program` %in% "ACT"),1)
# ACTMax <- tail(which(!is.na(data$`Testing Program`) & data$`Testing Program` %in% "ACT"),1)
# PARCCMin <- head(which(!is.na(data$`Testing Program`) & data$`Testing Program` %in% "PARCC"),1)
# PARCCMax <- tail(which(!is.na(data$`Testing Program`) & data$`Testing Program` %in% "PARCC"),1)
#   
# 
# 
# 
# define yAxisBreaks
yAxisFloor <- 150
yAxisCeiling <- 260
yAxisBreaks <- seq(from = yAxisFloor, to = yAxisCeiling, by = 10)

yAxisBreaksMin <- sum(head(yAxisBreaks, 2))/2
yAxisBreaksMax <- sum(tail(yAxisBreaks, 2))/2

# define y axis break parallel lines
gline <- linesGrob(y = c(0, 1),x = c(-.01, .01),  gp = gpar(col = "#2d2a26", lwd = 0.66))
# 
# # define legend position on x axis & y axis
# numberSates <- length(data$St)
# xAxisPoint <- round(numberSates * 9/17)
# yAxisPoint <- yAxisBreaksMin
# xAxisPointText <- xAxisPoint + 1
# yAxisPointText <- yAxisPoint
# 
# xAxisErrorBar <- round(numberSates * 4/5)
# yAxisErrorBarMin <- yAxisPoint - 2
# yAxisErrorBarMax <- yAxisPoint + 2
# xAxisErrorBarText <- xAxisErrorBar + 1
# yAxisErrorBarText <- yAxisPoint
# 
# 
# # define achievement level
# basic <- 208
# proficient <- 238


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
                       axis.line.x=element_line(color="#2d2a26", size = 0.235),
                       #axis.line.y set to be blank so the line breaks could have empty space
                       axis.line.y=element_blank(),
                       axis.text.x=element_text(color="#000000", size= 10),
                       axis.text.y=element_text(color="#000000", size= 10),
                       axis.ticks.x=element_blank(),
                       axis.ticks.y=element_line(color="#2d2a26", size= 0.235),
                       axis.ticks.length = unit(7.3,"points"),
                       plot.title=element_text(family="Open Sans", size= 10 ,lineheight=2, 
                                               color="#000000", face = "bold"),
                       aspect.ratio = 3.718/7.4328
                       )


# plotting
plot <- ggplot(data, aes(x = year, y = Highest)) +
  # draw hlines and the shades between them
  #scale_x_discrete() +
  geom_rect(aes(xmin = year-0.5, xmax = year+0.5, ymin = Lowest, ymax = Highest), fill = barCol) +
  
  geom_text(aes(y = Highest + 6, label = round(Highest, 0)), family="Open Sans", color = "#000000", size = 3.5 ) +
  geom_text(aes(y = Lowest - 5, label = round(Lowest, 0)), family="Open Sans", color = "#000000", size = 3.5) +
  geom_text(aes(y = Lowest + (Highest - Lowest)/2, label = round(range, 0)), family="Open Sans", fontface = "bold", color = "white", size = 3.5, vjust = 0.5) +
  
  # add axis titles
  labs(x = "Year", y = "", title = "NAEP equivalent score") +
  
  
  # apply themes 
  theme_bw() + 
  theme_general +
  
  
  # y axix and y axis break parallel lines
  scale_y_continuous(limits = c(yAxisFloor, yAxisCeiling), 
                     labels = c(0, yAxisBreaks[2:(length(yAxisBreaks) - 1)], 500), 
                     breaks = yAxisBreaks, expand = c(0,0)) +
  scale_x_discrete(limits = c(2007, 2015, 2017),
                     labels = c(2007, 2015, 2017),
                     breaks = c(2007, 2015, 2017) ) +
  coord_cartesian(clip = "off") +
  annotate("segment", x = -Inf, xend = -Inf, y = min(yAxisBreaks), yend = yAxisBreaksMin - 1, color = "#2d2a26", size = 0.235) +
  annotate("segment", x = -Inf, xend = -Inf, y = yAxisBreaksMin + 1, yend = yAxisBreaksMax - 1, color = "#2d2a26", size = 0.235) +
  annotate("segment", x = -Inf, xend = -Inf, y = yAxisBreaksMax + 1, yend = max(yAxisBreaks), color = "#2d2a26", size = 0.235) +
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
postscript(paste0("./Results/", "figure3_ex2", ".eps"), family = "Open Sans", width = 7.4328, height = 3.718) #width and height are in inches
grid::grid.draw(g)
dev.off()

