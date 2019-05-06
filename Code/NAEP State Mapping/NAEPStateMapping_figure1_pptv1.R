### NAEP State Mapping Figure
### 4/25/19
### Yuqi Liao



### Setting things up -----
reqpkg <- c("dplyr", "ggplot2", "readxl", "grid", "grDevices", "showtext", "extrafont", "lubridate")

sapply(reqpkg, function(pkgi) {
  if (!pkgi %in% installed.packages()) {
    install.packages(pkgi)
  }
  library(pkgi, character.only = TRUE)
})

wd <- "./Code/NAEP State Mapping"

setwd(wd)

### Loop through each tabs of the input spreadsheet -----
tabList <- c("R_G4", "M_G4", "R_G8", "M_G8")
yearList <- c("2017", "2015", "2007")

### Reading in data -----
for (tab in tabList){
  # read in data_2017, data_2015, and data_2007 for each grade/subject combination
  for (year in yearList) {
    data <- read_excel("./Materials/G4G8Figure1_190506_ppt.xlsx", sheet = paste0(tab, "_", year)) %>% 
      dplyr::select(`Consortia`, `st`, `NSE`, `Band +/-`, `NAEP Achivement Level`) %>% 
      #getting rid of irrelevant rows (if any)
      dplyr::filter(!`st` %in% c("AC", "SB", "PC", NA)) %>% 
      # # if `NSE` and `Band +/-` are 0, make them NA
      # mutate_at(.vars = vars(`NSE`, `Band +/-`),
      #           .funs = funs(ifelse(.==0, NA, .))) %>% 
      #calculate upperCut and lowerCut
      mutate(lowerCut = (`NSE` - `Band +/-`),
             upperCut = (`NSE` + `Band +/-`)) %>% 
      rename(midPoint = `NSE`) %>% 
      # # remove rows where midPoint is NA -**if year is 2017**
      # # reads: if 2017 is in `year`, do nothing, otherwise filter out NA rows
      # {if(grepl("2017", year)) .  else dplyr::filter(., !midPoint %in% NA)}
      
      # add a new column as the order for plotting (normally, if we plot the dots by state, each state will have three dots. now the clients don't want that, they want to have dots ordered from low to high for each year)
      mutate(orderNumber = row_number())

    # define order of st in each year's data frame
    order <- data$orderNumber
    data$orderNumber <- factor(data$orderNumber, levels = order)
    
    print(paste("Have read in data for: ", tab, year))
    glimpse(data)
    
    # assgin year-specific names to each data frame
    assign(paste0("data", "_", year) , data)
    
    # define colors based on subject and year (alpha is not support when exporting as eps, so have to define specific hex codes)
    if ( grepl("R", tab) ) {
      pointCol_2017 <- "#001871"
      lineCol_2017 <- "#001871"
      pointCol_2015 <- "#685da0"
      lineCol_2015 <- "#685da0"
      pointCol_2007 <- "#b3aacf"
      lineCol_2007 <- "#b3aacf"
    } else {
      pointCol_2017 <- "#C69214"
      lineCol_2017 <- "#C69214"
      pointCol_2015 <- "#dfb56a"
      lineCol_2015 <- "#dfb56a"
      pointCol_2007 <- "#f3d9b4"
      lineCol_2007 <- "#f3d9b4"
    }
  }


  # define achievement level for each grade/subject combination
  proficiencyLevelList <- read_excel("./Materials/G4G8Figure1_190425_ppt.xlsx", sheet = paste0(tab, "_", 2017)) %>% #tabs with 2017 has achievement level info
    dplyr::select(`ProficiencyLevel`, `Value`) %>% 
    na.omit() %>% 
    pull()
  
  basic <- proficiencyLevelList[1]
  proficient <- proficiencyLevelList[2]

  
  

  ### Plotting -----
  # # define index of testing programs - For PPT, decide not to add testing program lines for now
  # SBACMin <- head(which(!is.na(data$`Consortia`) & data$`Consortia` %in% "SBAC"),1)
  # SBACMax <- tail(which(!is.na(data$`Consortia`) & data$`Consortia` %in% "SBAC"),1)
  # ACTMin <- head(which(!is.na(data$`Consortia`) & data$`Consortia` %in% "ACT"),1)
  # ACTMax <- tail(which(!is.na(data$`Consortia`) & data$`Consortia` %in% "ACT"),1)
  # PARCCMin <- head(which(!is.na(data$`Consortia`) & data$`Consortia` %in% "PARCC"),1)
  # PARCCMax <- tail(which(!is.na(data$`Consortia`) & data$`Consortia` %in% "PARCC"),1)
  # 
  # # deal with special cases when the values have zero length (G8 Math)
  # if(length(PARCCMin) == 0) {
  #   PARCCMin <- NA
  # }
  # if(length(PARCCMax) == 0) {
  #   PARCCMax <- NA
  # }
    
  
  # define yAxisBreaks - based on year 2017
  if ( grepl("G4", tab) ) {
    yAxisFloor <- 160
    yAxisCeiling <- 270
  } else {
    yAxisFloor <- 220
    yAxisCeiling <- 330
  }

  yAxisBreaks <- seq(from = yAxisFloor, to = yAxisCeiling, by = 10)
  
  yAxisBreaksMin <- sum(head(yAxisBreaks, 2))/2
  yAxisBreaksMax <- sum(tail(yAxisBreaks, 2))/2
  
  # define y axis break parallel lines
  gline <- linesGrob(y = c(0, 1),x = c(-.01, .01),  gp = gpar(col = "#2d2a26", lwd = 0.66)) 
  
  # define legend position on x axis & y axis - based on year 2017
  numberSates <- length(data_2017$st)
  xAxisPoint <- round(numberSates * 0.54)
  yAxisPoint <- yAxisBreaksMin
  xAxisPointText <- xAxisPoint + numberSates/52
  yAxisPointText <- yAxisPoint
  
  xAxisErrorBar <- round(numberSates * 0.8)
  yAxisErrorBarMin <- yAxisPoint - 2
  yAxisErrorBarMax <- yAxisPoint + 2
  xAxisErrorBarText <- xAxisErrorBar + numberSates/52
  yAxisErrorBarText <- yAxisPoint
  
  
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
                         axis.text.x=element_blank(),
                         axis.text.y=element_text(color="#000000", size= 10),
                         axis.ticks.x=element_blank(),
                         axis.ticks.y=element_line(color="#2d2a26", size= 0.235),
                         axis.ticks.length = unit(7.3,"points"),
                         plot.title=element_text(family="Open Sans", size= 10 ,lineheight=2, 
                                                 color="#000000", face = "bold")
                         #aspect.ratio = 3.718/8
                         )
  
  
  # plotting
  plot <- ggplot() +
    # draw hlines and the shades between them
    scale_x_discrete() +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = basic, ymax = proficient), fill = "#ECECEC") +
    geom_hline(yintercept = basic, color = "#77787B", size = 0.235) +
    geom_hline(yintercept = proficient, color = "#77787B", size = 0.235) +
    
    # add achievement level text
    annotate("text", x = length(data$st)/2, y = basic - 5, label = paste0("NAEP~italic(Basic)~(", basic, ")"), 
             parse = TRUE, hjust = 0.5, color = "#77787B", size = 3.5, family="Open Sans") +
    annotate("text", x = length(data$st)/2, y = proficient + 5, label = paste0("NAEP~italic(Proficient)~(", proficient, ")"), 
             parse = TRUE, hjust = 0.5, color = "#77787B", size = 3.5, family="Open Sans") +
    
    # # draw error bars and point - 2017
    # geom_errorbar(aes(ymin = `lowerCut`, ymax = `upperCut`), size = 0.47, color = errorBarCol) +
    # # geom_text(aes(y = `lowerCut` - 2, label = st), family="Open Sans", color = "#000000", size = 2) + #no need for text abbrivations
    # geom_point(color = pointCol, size = 1) +
    
    # # draw error bars and point - 2017
    # geom_errorbar(data = data_2017, aes(x = orderNumber, ymin = `lowerCut`, ymax = `upperCut`), size = 0.47, color = errorBarCol) +
    # geom_point(data = data_2017, aes(x = orderNumber, y = midPoint), color = pointCol, size = 1) +
    # 
    # # draw error bars and point - 2015
    # geom_errorbar(data = data_2015, aes(x = orderNumber, ymin = `lowerCut`, ymax = `upperCut`), size = 0.47, color = errorBarCol, alpha = 0.55) +
    # geom_point(data = data_2015, aes(x = orderNumber, y = midPoint), color = pointCol, size = 1, alpha = 0.55) +
    # 
    # # draw error bars and point - 2007
    # geom_errorbar(data = data_2007, aes(x = orderNumber, ymin = `lowerCut`, ymax = `upperCut`), size = 0.47, color = errorBarCol, alpha = 0.4) +
    # geom_point(data = data_2007, aes(x = orderNumber, y = midPoint), color = pointCol, size = 1, alpha = 0.4) +
    
    # # add lines for testing program benchmarks - no need for now
    # annotate("segment", x = SBACMin - 0.4, xend = SBACMax + 0.4, y = data$`lowerCut`[SBACMin] - 5, yend = data$`lowerCut`[SBACMin] - 5 , color = "#2d2a26", size = 0.235) +
    # annotate("text", x = SBACMin + (SBACMax - SBACMin)/2, y = data$`lowerCut`[SBACMin] - 8, label = "SBAC", 
    #          hjust = 0.5, color = "#000000", size = 2.5, family="Open Sans") +
    # 
    # annotate("segment", x = ACTMin - 0.4, xend = ACTMax + 0.4, y = data$`lowerCut`[ACTMin] - 5, yend = data$`lowerCut`[ACTMin] - 5 , color = "#2d2a26", size = 0.235) +
    # annotate("text", x = ACTMin + (ACTMax - ACTMin)/2, y = data$`lowerCut`[ACTMin] - 8, label = "ACT", 
    #          hjust = 0.5, color = "#000000", size = 2.5, family="Open Sans") +
    # 
    # annotate("segment", x = PARCCMin - 0.4, xend = PARCCMax + 0.4, y = data$`lowerCut`[PARCCMin] - 5, yend = data$`lowerCut`[PARCCMin] - 5 , color = "#2d2a26", size = 0.235) +
    # annotate("text", x = PARCCMin + (PARCCMax - PARCCMin)/2, y = data$`lowerCut`[PARCCMin] - 8, label = "PARCC", 
    #          hjust = 0.5, color = "#000000", size = 2.5, family="Open Sans") +
    
    
    # add legend (fake data)
    geom_point(aes(x = xAxisPoint, y = yAxisPoint), color = pointCol_2017, size = 1) +
    annotate("text", x = xAxisPointText, y = yAxisPointText, family="Open Sans", 
              color = "#000000", size = 3.5, label = "NAEP equivalent score", hjust = 0) +
    geom_errorbar(aes( x = xAxisErrorBar, ymin = yAxisErrorBarMin, ymax = yAxisErrorBarMax), 
                  size = 0.47, color = errorBarCol_2017) +
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
    annotate("segment", x = -Inf, xend = -Inf, y = min(yAxisBreaks), yend = yAxisBreaksMin - 1, color = "#2d2a26", size = 0.235) +
    annotate("segment", x = -Inf, xend = -Inf, y = yAxisBreaksMin + 1, yend = yAxisBreaksMax - 1, color = "#2d2a26", size = 0.235) +
    annotate("segment", x = -Inf, xend = -Inf, y = yAxisBreaksMax + 1, yend = max(yAxisBreaks), color = "#2d2a26", size = 0.235) +
    annotation_custom(gline, ymin= yAxisBreaksMin - 2, ymax= yAxisBreaksMin, xmin= -Inf, xmax= Inf) + 
    annotation_custom(gline, ymin= yAxisBreaksMin, ymax= yAxisBreaksMin + 2, xmin= -Inf, xmax= Inf) +
    annotation_custom(gline, ymin= yAxisBreaksMax - 2, ymax= yAxisBreaksMax, xmin= -Inf, xmax= Inf) + 
    annotation_custom(gline, ymin= yAxisBreaksMax, ymax= yAxisBreaksMax + 2, xmin= -Inf, xmax= Inf) 
  
  # add lines and points
  # draw lines and point - 2007
  plot_2007 <- plot +     
    geom_line(data = data_2007, aes(x = orderNumber, y = midPoint, group = 1), size = 0.47, color = lineCol_2007) +
    geom_point(data = data_2007, aes(x = orderNumber, y = midPoint), color = pointCol_2007, size = 1)
  
  # move title to the left
  grid.newpage()
  g <- ggplotGrob(plot_2007)
  g$layout$l[g$layout$name == "title"] <- 4
  grid::grid.draw(g)
  
  ### Exporting
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
  postscript(paste0("./Results/", "PPT_figure1_2007", tab, "-",today(), ".eps"), family = "Open Sans", width = 7.667, height = 3.718) #width and height are in inches
  grid::grid.draw(g)
  dev.off()
    
  
  
  
  # draw error bars and point - 2015
  plot_2015 <- plot_2007 +     
    geom_line(data = data_2015, aes(x = orderNumber, y = midPoint, group = 1), size = 0.47, color = lineCol_2015) + 
    geom_point(data = data_2015, aes(x = orderNumber, y = midPoint), color = pointCol_2015, size = 1)
  
  # move title to the left
  grid.newpage()
  g <- ggplotGrob(plot_2015)
  g$layout$l[g$layout$name == "title"] <- 4
  grid::grid.draw(g)
  
  ### Exporting
  setEPS()
  postscript(paste0("./Results/", "PPT_figure1_2007_15", tab, "-",today(), ".eps"), family = "Open Sans", width = 7.667, height = 3.718) #width and height are in inches
  grid::grid.draw(g)
  dev.off()
  
  
  # draw error bars and point - 2017
  plot_2017 <- plot_2015 +     
    geom_line(data = data_2017, aes(x = orderNumber, y = midPoint, group = 1), size = 0.47, color = lineCol_2017) + 
    geom_point(data = data_2017, aes(x = orderNumber, y = midPoint), color = pointCol_2017, size = 1)
  
  # move title to the left
  grid.newpage()
  g <- ggplotGrob(plot_2017)
  g$layout$l[g$layout$name == "title"] <- 4
  grid::grid.draw(g)
  
  ### Exporting
  setEPS()
  postscript(paste0("./Results/", "PPT_figure1_2007_15_17", tab, "-",today(), ".eps"), family = "Open Sans", width = 7.667, height = 3.718) #width and height are in inches
  grid::grid.draw(g)
  dev.off()
  

  
} ### End of Loop -----

