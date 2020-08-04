### Census Pulse Survey data viz
### This is to be used as a gif tweet for the NCES handle
### 7/28/20
### Yuqi Liao


### Set things up ------
# #win
# install.packages("installr")
# library(installr)
# #mac
# library(devtools)
# install_github('andreacirilloac/updateR')
# library(updateR)
# updateR(admin_password = '')

# install.ImageMagick()
# Sys.setenv(PATH = paste("C:/PROGRA~1/ImageMagick-7.0.8-Q16",
#                         Sys.getenv("PATH"), sep = ";"))
# ani.options(convert = 'C:/PROGRA~1/ImageMagick-7.0.8-Q16/convert.exe')
# library(gganimate) # gh_install_packages("dgrtwo/gganimate", ref = "26ec501") #devtools::install_github("dgrtwo/gganimate")
# library(purrr)
# library(cowplot)
#library(ggtext)

# define and load all packages
reqpkg <- c("ggplot2","scales", "tidyr", "Cairo", "extrafont", "dplyr", "lubridate", "tweenr", "animation", "RColorBrewer", "grid", "gridExtra", "directlabels", "gganimate", "ggrepel", "showtext", "here", "stringr", "readxl", "cowplot","ggtext")

sapply(reqpkg, function(pkgi) {
  if (!pkgi %in% installed.packages()) {
    install.packages(pkgi, repos = "http://cran.us.r-project.org")
  }
  library(pkgi, character.only = TRUE)
})

# inspect the working directory
here()

#load font
##################################################################
## NOTE: only necessary if running the first time per R version!
# font_import() # takes a few minutes
# y
# font_import(paths = "/Users/Yuqi/Library/Fonts") #run only once
# y
##################################################################

extrafont::loadfonts() #didn't load "Gotham" font sucessfully, will stick with "Calibri" for now


### Read in data  -----
# the data has already been processed
data <- read_excel(path = here("Code", "Census", "Materials", "Figure 1_cleanData.xlsx"),
                   sheet = "Sheet1")
# data <- read_excel(path = here("Code", "COE", "Materials", "CTA-6_2020cleanData.xlsx"),
#                  sheet = "processed")


### Plotting ------
# Define order for `Category`
order <- data %>% 
  select(Category) %>% 
  pull(Category) 

data$Category <- factor(data$Category, levels = order)

# # fix labels
# data$labelRoundToZero <- ifelse(round(data$Percent,0) == 0, "#", round(data$Percent,0))
# 
# pushAsideThreshold <- 3
# data$labelNormal <- as.character(ifelse(data$Percent <= pushAsideThreshold, "", data$labelRoundToZero))
# # # not necessary here
# # data$labelNudge <- as.character(ifelse(data$Percent > pushAsideThreshold, "", data$labelRoundToZero))
# 


# color  
# nces_palette =  c("#fbab18", "#3EC7F4", "#3FA66C","#242953")
cols <- c("#071D49")

data0 <- data %>% mutate(Value = 0) 
# data2 <- data1 %>% mutate(Percent = ifelse(Gender %in% c(" Male  "), data$Percent, data1$Percent), df_id = "2")
# data3 <- data2 %>% mutate(Percent = ifelse(Gender %in% c(" Female "), data$Percent, data2$Percent), df_id = "3")
data1 <- data %>% mutate(Value = ifelse(Category %in% "Less than high school completion", data$Value, data0$Value)) 
data2 <- data %>% mutate(Value = ifelse(Category %in% "High school completion only", data$Value, data1$Value)) 
data3 <- data %>% mutate(Value = ifelse(Category %in% "Some college or associate's degree", data$Value, data2$Value)) 



# version 1 is for making all bars appear from 0 to values from the same time
ls <- list(data0, data)
tf <- tween_states(ls, tweenlength= 1, statelength=0, ease='cubic-in-out',nframes=100)

#version 2 is for making each bars appear from 0 to values one at a
ls2 <- list(data0, data1, data2, data3, data)
tf2 <- tween_states(ls2, tweenlength= 1, statelength=0, ease='cubic-in-out',nframes=200)

# define breaks
plotBreak = c(0, 2, 4, 6, 8, 10, 12, 14, 16)
#plotBreakLabel = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")
plotBreakLabel = plotBreak

# define title/caption, etc.
plotCaption <- "<span>SOURCE: U.S. Department of Commerce, Census Bureau, Household Pulse Survey.</span>"

plotSubtitle <- "Hours"
#plotSubtitle <- "Field of study"

plotTitle <- c("Adults' average weekly time spent on teaching activities with \nelementary and secondary students in their household, by \neducational attainment of adult: April 23 through May 5, 2020")
#plotTitle <- getWrappedText(plotTitle, width = 350, ps = 10)

# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(#aspect.ratio = 1.2:1,
                     text = element_text(family="PublicoText-Roman", color = "black"),
                     panel.background=element_blank(),
                     panel.border=element_rect(color="transparent"),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     axis.title.x=element_text(size=22, margin = margin(t=25, b = 15), hjust = .5, vjust = -1.1, color = "black", family = "PublicoText-Bold"),
                     axis.title.y=element_blank(),
                     axis.text.x=element_text(size=18, angle = 0, hjust = 0.5, vjust= -1, family = "PublicoText-Roman"),
                     axis.text.y=element_text(size=18, hjust = 1, family = "PublicoText-Roman"),
                     axis.line.x = element_blank(),
                     axis.line.y = element_blank(),
                     axis.ticks.x = element_line(size = 1, "black"),
                     axis.ticks.length =  unit(0, "cm"),
                     axis.ticks.y = element_blank(),
                     plot.title=element_text(size=26,family = "PublicoText-Bold", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
                     plot.subtitle=element_text(size=22,family = "PublicoText-Bold", hjust= 0, vjust = -1.1,lineheight=1, margin = margin(t = 15, b = 5), color = "black"),
                     plot.caption=element_markdown(size=17, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "PublicoText-Roman"),
                     plot.margin = unit(c(t = 0.3, r = 1, b = 0.3, l = 1), "cm"),
                     legend.position ="bottom",
                     legend.justification = "center",
                     legend.box.just = "left",
                     legend.title = element_blank(),
                     #legend.spacing = unit(c(1), "line"),
                     #legend.key.height=unit(1.5,"line"),
                     #legend.key.width=unit(1.5,"line"),
                     #legend.text = element_text(size=20, family = "PublicoText-Roman", hjust= 0,lineheight=1)
)

#ggrepelSeed <- 1234


# function to wrap x axis labels

autoWrap <- function(x) {
  str_wrap(x, width = 20)
}


# plotting (to see how it looks before animation)
plot <- ggplot(data = data, mapping = aes(x = Category, y = Value)) +
  geom_col(width = 0.9, fill = cols)  +
  #scale_fill_manual(values = cols) + 
  #coord_flip(clip = "off") +
  scale_y_continuous(breaks = plotBreak,
                     labels = plotBreakLabel,
                     expand = c(0, 0, 0, 0),
                     limits = c(0,17)) +
  scale_x_discrete(labels = autoWrap) +
  labs(x = "Educational attainment of adult", y = "", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
  theme_bw() +
  theme_white 
plot


# the below code to change plot layout (for better alignment) won't work with gganimate, so give up the gganimate for now
#grid.newpage()
# ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
g <- ggplotGrob(plot)
g$layout$l[g$layout$name == "title"] <- 4
g$layout$l[g$layout$name == "caption"] <- 4
g$layout$l[g$layout$name == "subtitle"] <- 4
#g$layout$l[g$layout$name == "guide-box"] <- 4
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

# # function to draw the same figure n-many times, so that the animation "pauses"
gifReplicate <- function(x) {
  grid.draw(x)
  grid.newpage()
}


# create GIF - version: pause in each year ----- 

saveGIF({
  print(Sys.time())
  for (i in 1:max(tf2$.frame)) {
    print(paste0("working on the ", i, "th frame"))
    g <- ggplot(data = subset(tf2, .frame == i), mapping = aes(x = Category, y = Value, .frame = i)) +
      geom_col(fill = cols, width = 0.9)  +
      scale_y_continuous(breaks = plotBreak,
                         labels = plotBreakLabel,
                         expand = c(0, 0, 0, 0),
                         limits = c(0,17)) +
      scale_x_discrete(labels = autoWrap) +
      labs(x = "Educational attainment of adult", y = "", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
      theme_bw() +
      theme_white 
   

    
    #add label for the 1st group
    if(i > 50){
      g <- g + geom_text(data = subset(tf2, .frame == i & Category == data$Category[1]),
                         mapping = aes(x = Category, y = Value, label = round(Value, 1)),
                         color = "black", size = 8,
                         position = position_stack(vjust = 1.07),
                         family="PublicoText-Roman")
    }
    #add label for the 2nd group
    if(i > 100){
      g <- g + geom_text(data = subset(tf2, .frame == i & Category == data$Category[2]),
                         mapping = aes(x = Category, y = Value, label = round(Value, 1)),
                         color = "black", size = 8,
                         position = position_stack(vjust = 1.07),
                         family="PublicoText-Roman")
    }
    #add label for the 3rd group
    if(i > 150){
      g <- g + geom_text(data = subset(tf2, .frame == i & Category == data$Category[3]),
                         mapping = aes(x = Category, y = Value, label = round(Value, 1)),
                         color = "black", size = 8,
                         position = position_stack(vjust = 1.07),
                         family="PublicoText-Roman")
    }
    #add label for the 4th group
    if(i == 200){
      g <- g + geom_text(data = subset(tf2, .frame == i & Category == data$Category[4]),
                         mapping = aes(x = Category, y = Value, label = round(Value, 1)),
                         color = "black", size = 8,
                         position = position_stack(vjust = 1.07),
                         family="PublicoText-Roman")
    }
      
       
    
    
    g <- ggplotGrob(g)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    #g$layout$l[g$layout$name == "guide-box"] <- 4
    
    # # if i <= 51 (aka when i is still showing the first level "Male"), cover the legend box for "Female"
    # if (i <= 51) {
    #   
    #   g2 <- ggdraw(g) + 
    #     geom_rect(aes(xmin = 0.58, xmax = 0.68, ymin = 0.09, ymax = 0.15),
    #               colour = "white", fill = "white")
    # } else {
    #   g2 <- ggdraw(g)
    # }
    
    # g2 <- ggdraw(g)
    g2 <- g

    # grid::grid.draw(g);
    # grid.newpage()
    
    # define object that store the name of frame with which we want the plot to "pause"
   # pause_frames <- tf %>% group_by(df_id) %>% slice(max(row_number(.frame))) %>% pull(.frame) #this is not correct it turns out
    pause_frames <- c(200)
    
   
    
    # draw the plot
      # replicate (and draw) the plot many times
      if (i == pause_frames[length(pause_frames)]){
        replicate(300, gifReplicate(g2))
        #because `gifReplicate` will end with a new page, i can just draw g2 one more time here to avoid having to manually remove that new blank page later
        grid::grid.draw(g2)
      }  else {
        # just draw the plot one time 
        grid::grid.draw(g2)
        grid.newpage()
    }
    
  }
  print(Sys.time())
},
# specify the pathway and name of the gif output, as well as the interval, width, and height
movie.name=here("Code", "Census", "Results", "Figure1-5.gif"),interval = .02, ani.width = 900, ani.height = 900) #unfortunately, when `ggdraw` is used, the first time grid::grid.draw(g2) is run, there will be a blank page saved into the graphic device. my  solution is to manually delete the first blank frame in Photoshop after the compressed gif is generated. I previously tried to add an if statement to use grid::grid.draw(g) (instead of g2) for the first frame; but it is not a perfect solution since i can't add a white sqaure on top of it. 
#compressing	
gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){	
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)	
  system.fun <- if (.Platform$OS.type == "windows") shell else system	
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))	
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))	
}	

gif_compress("/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/Census/Results/Figure1-5.gif","/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/Census/Results/Figure1-5_compressed.gif")

 




