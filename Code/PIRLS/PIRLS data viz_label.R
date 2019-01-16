### PIRLS data viz
### This is to be used as a tweet for the NCES handle
### 10/12/18
### Yuqi Liao


### Set things up ------
## install gganimate and its dependents
# install.packages("installr")
# library(installr)
# install.ImageMagick()

devtools::load_all("U:/ESSIN Task 14/NAEP R Program/Yuqi/edsurvey")
library(ggplot2)
library(scales)
library(tidyr)
library(Cairo)
library(extrafont)
library(dplyr)
library(lubridate)
library(tweenr)
library(gganimate) # gh_install_packages("dgrtwo/gganimate", ref = "26ec501") #devtools::install_github("dgrtwo/gganimate")
library(animation)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(directlabels)
library(purrr)
library(gganimate)
library(ggrepel)
library(showtext)
library(ggplotify)
library(cowplot)

# Sys.setenv(PATH = paste("C:/PROGRA~1/ImageMagick-7.0.8-Q16",
#                         Sys.getenv("PATH"), sep = ";"))
# ani.options(convert = 'C:/PROGRA~1/ImageMagick-7.0.8-Q16/convert.exe')




### download/clean data  -----
#download_ePIRLS(years = c(2016), root = "./Data/", cache = FALSE, verbose = TRUE)

eP16 <- read_ePIRLS(path = "./Data/ePIRLS/2016/", countries = "*")

showCutPoints(data = eP16$datalist[[1]])

aLev1 <- achievementLevels(achievementVars = c("erea"),
                           data = eP16$datalist[[1]], returnDiscrete = TRUE)


GetAchLevel <- function(x, y){
  AchLevel <- achievementLevels(achievementVars = c("erea"),
                                data = x, returnDiscrete = TRUE)
  
  AchLevel <- AchLevel$discrete %>% dplyr::mutate(Country = y)
  
}

AchLevels <- purrr::map2(eP16$datalist, eP16$covs$country, GetAchLevel)

#combine all list (each country is one list)
df <- do.call(rbind.data.frame, AchLevels)


df <- df %>% 
  # drop the benchmarking education systems for now
  filter(!Country %in% c("Abu Dhabi, UAE", "Dubai, UAE")) %>% 
  select(Percent, Country, Level)

### Plotting ------
# CountryOrder
CountryOrder <- df %>% 
  group_by(Level) %>% 
  arrange(Percent) %>% 
  mutate(RankOrder = row_number()) %>% 
  filter(Level %in% ("At Advanced International Benchmark")) %>% 
  ungroup() %>% 
  pull(Country) #get a vector from a tbl

df$Country <- factor(df$Country, levels = CountryOrder)

# Level order
df$Level <- factor(df$Level, levels = levels(df$Level), 
                   labels = c(" Below Low  ", " Low  ", " Intermediate  ", " High  ", " Advanced  "))

# fix labels
df$labelRoundToZero <- ifelse(round(df$Percent,0) == 0, "#", round(df$Percent,0))
pushAsideThreshold <- 3
df$labelNormal <- as.character(ifelse(df$Percent <= pushAsideThreshold, "", df$labelRoundToZero))
df$labelNudge <- as.character(ifelse(df$Percent > pushAsideThreshold, "", df$labelRoundToZero))





# color  
cols <- c("#982F3A", "#B3B3B3", "#3D3629", "#BA8752", "#143875")
#cols <- c("#FFFFFF","#143875", "#BA8752", "#3D3629", "#B3B3B3", "#982F3A")

df1 <- df %>% mutate(Percent = 0, df_id = "1") 
df2 <- df1 %>% mutate(Percent = ifelse(Level %in% c(" Advanced  "), df$Percent, df1$Percent), df_id = "2")
df3 <- df2 %>% mutate(Percent = ifelse(Level %in% c(" High  "), df$Percent, df2$Percent), df_id = "3")
df4 <- df3 %>% mutate(Percent = ifelse(Level %in% c(" Intermediate  "), df$Percent, df3$Percent), df_id = "4")
df5 <- df4 %>% mutate(Percent = ifelse(Level %in% c(" Low  "), df$Percent, df4$Percent), df_id = "5")
df6 <- df4 %>% mutate(Percent = ifelse(Level %in% c(" Below Low  "), df$Percent, df5$Percent), df_id ="6" )

ls <- list(df1, df2, df3, df4, df5, df6)

tf <- tween_states(ls, tweenlength= 1, statelength=0, ease='cubic-in-out',nframes=100)



plotCaption <- expression(atop('NOTE: Education systems are ordered by the percentage of students reaching the '~italic('Advanced')~' international benchmark.                                                    ','SOURCE: International Association for the Evaluation of Educational Achievement (IEA), Progress in International Reading Literacy Study (PIRLS), 2016.'))

# plotCaption <- list()
# plotNote <- expression(paste("NOTE: Education systems are ordered by the percentage of students reaching the ", italic("Advanced")," international benchmark."))
# plotSource <- c("\nSOURCE: International Association for the Evaluation of Educational Achievement (IEA), Progress in International Reading Literacy Study (PIRLS), 2016.")
# plotCaption <- c(plotNote, plotSource)
# plotCaption <- paste(plotCaption, collapse = "\n")
# 
# plotCaption <- getWrappedText(plotCaption, width = 400, ps = 10)
# 
plotSubtitle <- "Education system"

plotTitle <- c("Percentage of fourth-grade students reaching the ePIRLS \ninternational benchmarks in online informational reading, \nby education system: 2016")
#plotTitle <- getWrappedText(plotTitle, width = 350, ps = 10)

# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(#aspect.ratio = 1.2:1,
                     text = element_text(size=16, family="Calibri", color = "black"),
                     panel.background=element_blank(),
                     panel.border=element_rect(color="transparent"),
                     panel.grid = element_blank(),
                     axis.title.x=element_text(size=18, margin = margin(t=15, b = 5), hjust = .5),
                     #axis.title.y=element_text(size=10, margin = margin(t=0, b = 5),hjust = 0,vjust = 1, angle = 0),
                     axis.text.x=element_text(size=16, angle = 0, hjust = 0.5, family = "Calibri"),
                     axis.text.y=element_text(size=16, hjust = 0, family = "Calibri", face = "bold", color = "black"),
                     axis.line.x = element_line(size = 1),
                     axis.line.y = element_blank(),
                     axis.ticks.x = element_line(size = 1),
                     axis.ticks.length =  unit(.25, "cm"),
                     axis.ticks.y = element_blank(),
                     plot.title=element_text(size=35,family = "Calibri", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
                     plot.subtitle=element_text(size=25,family = "Calibri", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15, b = 5)),
                     plot.caption=element_text(size=15, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "Calibri"),
                     plot.margin = unit(c(t = 0.3, r = 1, b = 0.3, l = 1), "cm"),
                     legend.position ="bottom",
                     legend.justification = "left",
                     legend.box.just = "left",
                     legend.title = element_blank(),
                     #legend.spacing = unit(c(1), "line"),
                     legend.key.height=unit(1,"line"),
                     legend.key.width=unit(1,"line"),
                     legend.text = element_text(size=20, family = "Calibri", hjust= 0,lineheight=1)
)




# plotting
plot <- ggplot(data = df, mapping = aes(x = Country, y = Percent, fill = Level)) +
  geom_col(width = 0.6)  +
  scale_fill_manual(values = cols, breaks = rev(levels(df$Level))) + 
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c(0, 20, 40, 60, 80, 100),
                     expand = c(0, 0, 0, 0)) +
  labs(x = "", y = "Percent", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
  theme_bw() +
  theme_white 
plot


# the below code to change plot layout (for better alignment) won't work with gganimate, so give up the gganimate for now
grid.newpage()
# ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
g <- ggplotGrob(plot)
g$layout$l[g$layout$name == "title"] <- 4
g$layout$l[g$layout$name == "caption"] <- 4
g$layout$l[g$layout$name == "subtitle"] <- 4
g$layout$l[g$layout$name == "guide-box"] <- 4
grid::grid.draw(g);


### Animation ------
## This code are for when I don't need to use ggplotGrob so I can use gganimate directly
# animation <- plot +
#   transition_states(Country, wrap = FALSE, transition_length = 1, state_length = 1) +
#   shadow_mark() +
#   enter_fade() +
#   #enter_drift(x_mod = 0, y_mod = -8) +
#   ease_aes('cubic-in-out')
# animation
# # access metat data about the frames in an animation
# meta <- frame_vars()
# # use animate from animation to achieve end_pause
# ani <- animate(animation, nframes = 100, fps = 20, end_pause = 10, rewind = FALSE)
# anim_save(filename = "pirls.gif", animation = last_animation())
# clean the original data frame

# function to draw the same figure n-many times, so that the animation "pauses"
gifReplicate <- function(x) {
  grid.newpage()
  grid.draw(x)
}

# create GIF - version: pause at start/end ----- 
saveGIF({
  print(Sys.time())
  firstFig <-  ggplot(data = tf, mapping = aes(x = Country, y = Percent)) +
    geom_col(data = subset(tf, .frame == min(.frame)), 
             aes(fill = Level),
             width = 0.6)  +
    scale_fill_manual(values = cols, breaks = rev(levels(df$Level))) + 
    coord_flip(clip = "off") +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       labels = c(0, 20, 40, 60, 80, 100),
                       expand = c(0, 0, 0, 0),
                       limits = c(0,100)) +
    labs(x = "", y = "Percent", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
    theme_bw() +
    theme_white 
 
  # ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
  g <- ggplotGrob(firstFig)
  g$layout$l[g$layout$name == "title"] <- 4
  g$layout$l[g$layout$name == "caption"] <- 4
  g$layout$l[g$layout$name == "subtitle"] <- 4
  g$layout$l[g$layout$name == "guide-box"] <- 4
  grid::grid.draw(g);
  
  # replicate the same figure n-many times, so that the animation "pauses" 
  replicate(30,gifReplicate(g))
  grid.newpage()
  
  # from the 1st to the max frame of the full length of the data set, take the i-th value and generate a visualization
  for (i in 1:max(tf$.frame)) {
    print(paste0("working on the ", i, "th frame"))
    g <- ggplot(data = subset(tf, .frame == i), mapping = aes(x = Country, y = Percent, .frame = i)) +
      geom_col(aes(fill = Level),
               width = 0.6)  +
      scale_fill_manual(values = cols, breaks = rev(levels(df$Level))) + 
      coord_flip(clip = "off") +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                         labels = c(0, 20, 40, 60, 80, 100),
                         expand = c(0, 0, 0, 0),
                         limits = c(0,100)) +
      labs(x = "", y = "Percent", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
      theme_bw() +
      theme_white 

    g <- ggplotGrob(g)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    g$layout$l[g$layout$name == "guide-box"] <- 4
    grid::grid.draw(g);
    grid.newpage()
  
  }
  
  # the full figure
  wholeFig <- ggplot(data = tf, mapping = aes(x = Country, y = Percent)) +
    geom_col(data = subset(tf, .frame == max(.frame)), 
             aes(fill = Level),
             width = 0.6)  +
    scale_fill_manual(values = cols, breaks = rev(levels(df$Level))) + 
    coord_flip(clip = "off") +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       labels = c(0, 20, 40, 60, 80, 100),
                       expand = c(0, 0, 0, 0),
                       limits = c(0,100)) +
    labs(x = "", y = "Percent", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
    theme_bw() +
    theme_white 
  
  # ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
  g <- ggplotGrob(wholeFig)
  g$layout$l[g$layout$name == "title"] <- 4
  g$layout$l[g$layout$name == "caption"] <- 4
  g$layout$l[g$layout$name == "subtitle"] <- 4
  g$layout$l[g$layout$name == "guide-box"] <- 4
  grid::grid.draw(g);
  
  # replicate the same figure n-many times, so that the animation "pauses" 
  replicate(250,gifReplicate(g))
  print(Sys.time())
},
# specify the pathway and name of the gif output, as well as the interval, width, and height
movie.name="experiment.gif",interval = .02, ani.width = 1200, ani.height = 1000)

# create GIF - version: pause in each year ----- 

saveGIF({
  print(Sys.time())
  for (i in 1:max(tf$.frame)) {
    print(paste0("working on the ", i, "th frame"))
    g <- ggplot(data = subset(tf, .frame == i), mapping = aes(x = Country, y = Percent, .frame = i)) +
      geom_col(aes(fill = Level),
               width = 0.6)  +
      scale_fill_manual(values = cols, breaks = rev(levels(df$Level))) + 
      coord_flip(clip = "off") +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                         labels = c(0, 20, 40, 60, 80, 100),
                         expand = c(0, 0, 0, 0),
                         limits = c(0,100)) +
      labs(x = "", y = "Percent", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
      theme_bw() +
      theme_white +
    
    #add white label (so it's invisible at earlier frames)
        #labelNormal
        geom_text_repel(data = subset(tf, .frame == max(tf$.frame)), 
                        mapping = aes(x = Country, y = Percent, label = labelNormal, group = Level),
                        position = position_stack(vjust = 0.5), 
                        color = "white", size = 5,
                        point.padding = NA,
                        seed = ggrepelSeed, direction = "x", force = 2, show.legend = FALSE) +
        #labelNudge 
        geom_text_repel(data = subset(tf, .frame == max(tf$.frame)), 
                        mapping = aes(x = Country, y = Percent, label = labelNudge, group = Level),
                        color = "white",size = 5,
                        position = position_stack(vjust = 0.5), hjust = -2, seed = ggrepelSeed, direction = "x", force = 2, show.legend = FALSE)
    
    
    g <- ggplotGrob(g)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    g$layout$l[g$layout$name == "guide-box"] <- 4
    grid::grid.draw(g);
    grid.newpage()
    
    # define object that store the name of frame with which we want the plot to "pause"
   # pause_frames <- tf %>% group_by(df_id) %>% slice(max(row_number(.frame))) %>% pull(.frame) #this is not correct it turns out
    pause_frames <- c(21, 41, 61, 81, 100)
    
    # draw the plot
    if (i %in% pause_frames){
      # replicate (and draw) the plot many times
      grid::grid.draw(g);
      # let the last plot pause a bit more
      if (i == pause_frames[length(pause_frames)]){
        replicate(200,gifReplicate(g))
      } else {
        replicate(30,gifReplicate(g))
      }
      
    } else {
      # just draw the plot one time
      grid::grid.draw(g);
      grid.newpage()
    }
    
  }
  print(Sys.time())
},
# specify the pathway and name of the gif output, as well as the interval, width, and height
movie.name="experiment_v15.gif",interval = .02, ani.width = 1080, ani.height = 900)





