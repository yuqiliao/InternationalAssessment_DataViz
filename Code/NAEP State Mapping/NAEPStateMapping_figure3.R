### NAEP State Mapping Figure
### 1/16/19
### Yuqi Liao, Michael Lee, & Howard Huo
### Updated on 5/9/24 for the report in 2024



### Setting things up -----
reqpkg <- c("dplyr", "ggplot2", "readxl", "grid", "grDevices", "showtext", "extrafont", "scales", "lubridate")

sapply(reqpkg, function(pkgi) {
  if (!pkgi %in% installed.packages()) {
    install.packages(pkgi)
  }
  library(pkgi, character.only = TRUE)
})

wd <- ".Code/NAEP State Mapping"

setwd(wd)

### Loop through each tabs of the input spreadsheet -----
tabList <- c("R_G4", "M_G4", "R_G8", "M_G8")

### Reading in data -----
for (tab in tabList){
  data <- read_excel("./Materials/G4G8Figure3_06032024_yl.xlsx", sheet = tab) %>% 
  dplyr::select(`year`, `Highest`, `Lowest`, `range`)
  
  # define achievement level
  proficiencyLevelList <- read_excel("./Materials/G4G8Figure1_06032024_yl.xlsx", sheet = tab) %>% 
    dplyr::select(`ProficiencyLevel`, `Value`) %>% 
    na.omit() %>% 
    pull()
  
  basic <- proficiencyLevelList[1]
  proficient <- proficiencyLevelList[2]
  
  # # add dummy data (years) for placeholder #decided to use scale_x_continuous for now so this is not needed
  # data <- data %>% 
  #   add_row(year = 2009:2010, Highest = c(NA, NA), 
  #           Lowest = c(NA, NA), range = c(NA, NA), .before = 2)
  # 
  # # define order of year (turn x-axis into a categorical variable)
  # orderYear <- sort(data$year)
  # data$year <- factor(data$year, levels = orderYear)
  
  glimpse(data)
  ### Plotting -----
  # define color
  if ( grepl("R", tab) ) {
    barCol <- "#212C68"
  } else {
    barCol <- "#007167"
  }
  
  # define yAxisBreaks
  if ( grepl("G4", tab) ) {
    if(grepl("R", tab) ){
      yAxisFloor <- 150
      yAxisCeiling <- 260
    } else {
      yAxisFloor <- 170
      yAxisCeiling <- 280
    }
  } else {
    if(grepl("R", tab) ){
      yAxisFloor <- 200
      yAxisCeiling <- 310
    } else {
      yAxisFloor <- 220
      yAxisCeiling <- 330
    }
  }
  
  yAxisBreaks <- seq(from = yAxisFloor, to = yAxisCeiling, by = 10)
  
  yAxisBreaksMin <- sum(head(yAxisBreaks, 2))/2
  yAxisBreaksMax <- sum(tail(yAxisBreaks, 2))/2
  
  # define y axis break parallel lines
  gline <- linesGrob(y = c(0, 1),x = c(-.021, .021),  gp = gpar(col = "#2d2a26", lwd = 0.66))
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
  
  # define functions to squish space between empty years [reference: https://stackoverflow.com/questions/47234710/how-can-i-remove-part-of-y-axis-and-reverse-the-axis-in-ggplot2]
  # update 05/09/24: based on my note: it looks like this function is no longer working/used.
  # squish_trans <- function(from, to, factor) { 
  #   
  #   trans <- function(x) {    
  #     # get indices for the relevant regions
  #     isq <- x > from & x < to
  #     ito <- x >= to
  #     
  #     # apply transformation
  #     x[isq] <- from + (x[isq] - from)/factor
  #     x[ito] <- from + (to - from)/factor + (x[ito] - to)
  #     
  #     return(x)
  #   }
  #   
  #   inv <- function(x) {
  #     
  #     # get indices for the relevant regions
  #     isq <- x > from & x < from + (to - from)/factor
  #     ito <- x >= from + (to - from)/factor
  #     
  #     # apply transformation
  #     x[isq] <- from + (x[isq] - from) * factor
  #     x[ito] <- to + (x[ito] - (from + (to - from)/factor))
  #     
  #     return(x)
  #   }
  #   
  #   # return the transformation
  #   return(trans_new("squished", trans, inv))
  # }
  
  
  # load font
  # font_add_google("Open Sans")
  # showtext_auto()
  
  # define theme_general
  theme_general <- theme(text=element_text(family="Open Sans", color = "#000000"),
                         panel.background=element_blank(),
                         panel.border=element_rect(color="transparent"),
                         plot.margin = unit(c(0,0,0,0), "npc"),
                         panel.grid.major.y=element_blank(),
                         panel.grid.major.x=element_blank(),
                         panel.grid.minor.x=element_blank(),
                         panel.grid.minor.y=element_blank(),
                         axis.title.x = element_text(family="Open Sans", color = "#000000", 
                                                   size = 10, face = "bold"),
                         axis.line.x=element_line(color="#2d2a26", size = 0.235),
                         #axis.line.y set to be blank so the line breaks could have empty space
                         axis.line.y=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_text(color="#000000", size= 10),
                         axis.ticks.x=element_blank(),
                         axis.ticks.y=element_line(color="#2d2a26", size= 0.235),
                         axis.ticks.length = unit(7.3,"points"),
                         plot.title=element_text(family="Open Sans", size= 10 ,lineheight=2, 
                                                 color="#000000", face = "bold")
                         #aspect.ratio = 3.718/7.4328
                         )
  
  
  # plotting
  plot <- ggplot(data, aes(x = year, y = Highest)) +
    
    # add achievement level line
    geom_hline(yintercept = basic, color = "#77787B", size = 0.235) +
    geom_hline(yintercept = proficient, color = "#77787B", size = 0.235) +

    # # add achievement level text - version 1
    # # (normally the x position would be the center year (2007+2017)/2 = 2012, but because I use the squish_trans function later, here I have to manually adjust the x position to about 2014.2 to make it center. However I ended up using 2012 for now so the text is not overlapped with the bar )
    # annotate("text", x = 2012, y = basic - 5, label = paste0("NAEP~italic(Basic)~(", basic, ")"),
    #          parse = TRUE, hjust = 0.5, color = "#77787B", size = 3.5, family="Open Sans") +
    # annotate("text", x = 2012 , y = proficient + 5, label = paste0("NAEP~italic(Proficient)~(", proficient, ")"),
    #          parse = TRUE, hjust = 0.5, color = "#77787B", size = 3.5, family="Open Sans") +
    
    
    # add achievement level text - version 2
    annotate("text", x = 2016, y = basic - 5, label = paste0("(", basic, ")"), 
             parse = TRUE, hjust = 0.5, color = "#77787B", size = 3.5, family="Open Sans") +
    annotate("text", x = 2016, y = basic + 5, label = paste0("NAEP~italic(Basic)"), 
             parse = TRUE, hjust = 0.5, color = "#77787B", size = 3.5, family="Open Sans") +
    annotate("text", x = 2016 , y = proficient - 5, label = paste0("(", proficient, ")"), 
             parse = TRUE, hjust = 0.5, color = "#77787B", size = 3.5, family="Open Sans") +
    annotate("text", x = 2016 , y = proficient + 5, label = paste0("NAEP~italic(Proficient)"), 
             parse = TRUE, hjust = 0.5, color = "#77787B", size = 3.5, family="Open Sans") +
    
    #scale_x_discrete() +
    geom_rect(aes(xmin = year-0.3, xmax = year+0.3, ymin = Lowest, ymax = Highest), fill = barCol) +
    
    geom_text(aes(y = Highest + 6, label = round(Highest, 0)), family="Open Sans", color = barCol, size = 3.5 ) +
    geom_text(aes(y = Lowest - 5, label = round(Lowest, 0)), family="Open Sans", color = barCol, size = 3.5) +
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
    scale_x_continuous(#limits = c(2006, 2018),
                       labels = c(2013, 2019, 2022),
                       breaks = c(2013, 2019, 2022)) + #YL: the years here are not chagning anything, the year positions are determined in the `geom_rect` call above
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
  postscript(paste0("./Results/", today(), "-", "figure3", tab, ".eps"), family = "Open Sans", width = 3.8, height = 3.718) #width and height are in inches
  #add save as png for review
  ggsave(paste0("./Results/", today(), "-", "figure3", tab, ".png"), width = 3.8, height = 3.718)
  grid::grid.draw(g)
  dev.off()

} ### End of Loop -----
