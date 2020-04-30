### COE indicators data viz
### This is to be used as a gif tweet for the NCES handle
### 4/29/20 update
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
library(ggtext)

# define and load all packages
reqpkg <- c("ggplot2","scales", "tidyr", "Cairo", "extrafont", "dplyr", "lubridate", "tweenr", "animation", "RColorBrewer", "grid", "gridExtra", "directlabels", "gganimate", "ggrepel", "showtext", "here", "stringr", "readxl", "cowplot")

sapply(reqpkg, function(pkgi) {
  if (!pkgi %in% installed.packages()) {
    install.packages(pkgi, repos = "http://cran.us.r-project.org")
  }
  library(pkgi, character.only = TRUE)
})

# inspect the working directory
here()

#load font
#font_import(paths = "/Users/Yuqi/Library/Fonts") #run only once
loadfonts() #didn't load "Gotham" font sucessfully, will stick with "Calibri" for now


### Read in data  -----
# the data has already been processed, based on the raw data from "tabn322.40.xls" and "tabn322.50.xls"
data <- read_excel(path = here("Code", "COE", "Materials", "CTA-6_2020cleanData.xlsx"),
                 sheet = "processed")


### Plotting ------
# Define order for `Category` (field of study)
order <- data %>% 
  group_by(Gender) %>% 
  arrange(desc(Percent)) %>% 
  mutate(RankOrder = row_number()) %>% 
  # ordered by Male's percentage from low to high
  filter(Gender %in% ("Male")) %>% 
  ungroup() %>%
  # to get a vector from a tbl
  pull(Category) 

data$Category <- factor(data$Category, levels = order)


# Define order for `Gender`
data$Gender <- factor(data$Gender, labels = c(" Female "," Male  "))


# fix labels
data$labelRoundToZero <- ifelse(round(data$Percent,0) == 0, "#", round(data$Percent,0))

pushAsideThreshold <- 3
data$labelNormal <- as.character(ifelse(data$Percent <= pushAsideThreshold, "", data$labelRoundToZero))
# # not necessary here
# data$labelNudge <- as.character(ifelse(data$Percent > pushAsideThreshold, "", data$labelRoundToZero))



# color  
# nces_palette =  c("#fbab18", "#3EC7F4", "#3FA66C","#242953")
cols <- c("#fbab18", "#3EC7F4")

data1 <- data %>% mutate(Percent = 0, df_id = "1") 
data2 <- data1 %>% mutate(Percent = ifelse(Gender %in% c(" Male  "), data$Percent, data1$Percent), df_id = "2")
data3 <- data2 %>% mutate(Percent = ifelse(Gender %in% c(" Female "), data$Percent, data2$Percent), df_id = "3")

ls <- list(data1, data2, data3)

tf <- tween_states(ls, tweenlength= 1, statelength=0, ease='cubic-in-out',nframes=100)

# define breaks
plotBreak = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
#plotBreakLabel = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")
plotBreakLabel = plotBreak

# define title/caption, etc.
plotCaption <- "<span>SOURCE: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS),<br>Fall 2018, Completions component. See <i style='font-family: PublicoText-Italic'>Digest of Education Statistics 2019,</i> tables 322.40 and 322.50.</span>"

# plotCaption <- list()
# plotNote <- expression(paste("NOTE: Education systems are ordered by the percentage of students reaching the ", italic("Advanced")," international benchmark."))
# plotSource <- c("\nSOURCE: International Association for the Evaluation of Educational Achievement (IEA), Progress in International Reading Literacy Study (PIRLS), 2016.")
# plotCaption <- c(plotNote, plotSource)
# plotCaption <- paste(plotCaption, collapse = "\n")
# 
# plotCaption <- getWrappedText(plotCaption, width = 400, ps = 10)
# 

#plotSubtitle <- "Education system"
plotSubtitle <- "Field of study"

plotTitle <- c("Percentage distribution of bachelor's degrees conferred by postsecondary \ninstitutions in selected fields of study, by sex: Academic year 2017-18")
#plotTitle <- getWrappedText(plotTitle, width = 350, ps = 10)

# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(#aspect.ratio = 1.2:1,
                     text = element_text(size=16, family="PublicoText-Roman", color = "black"),
                     panel.background=element_blank(),
                     panel.border=element_rect(color="transparent"),
                     panel.grid = element_blank(),
                     axis.title.x=element_text(size=24, margin = margin(t=15, b = 5), hjust = .5, color = "#686868"),
                     #axis.title.y=element_text(size=10, margin = margin(t=0, b = 5),hjust = 0,vjust = 1, angle = 0),
                     axis.text.x=element_text(size=18, angle = 0, hjust = 0.5, family = "PublicoText-Roman"),
                     axis.text.y=element_text(size=20, hjust = 1, family = "PublicoText-Roman", face = "plain", color = "black"),
                     axis.line.x = element_line(size = 1, color = "#686868"),
                     axis.line.y = element_blank(),
                     axis.ticks.x = element_line(size = 1, "#686868"),
                     axis.ticks.length =  unit(.25, "cm"),
                     axis.ticks.y = element_blank(),
                     plot.title=element_text(size=31,family = "PublicoText-Bold", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
                     plot.subtitle=element_text(size=24,family = "PublicoText-Roman", face = "plain" , hjust= 0,lineheight=1, margin = margin(t = 15, b = 5), color = "#686868"),
                     plot.caption=element_markdown(size=16, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "PublicoText-Roman"),
                     plot.margin = unit(c(t = 0.3, r = 1, b = 0.3, l = 1), "cm"),
                     legend.position ="bottom",
                     legend.justification = "center",
                     legend.box.just = "left",
                     legend.title = element_blank(),
                     #legend.spacing = unit(c(1), "line"),
                     legend.key.height=unit(1.5,"line"),
                     legend.key.width=unit(1.5,"line"),
                     legend.text = element_text(size=21, family = "PublicoText-Roman", hjust= 0,lineheight=1)
)

ggrepelSeed <- 1234


# function to wrap x axis labels

autoWrap <- function(x) {
  str_wrap(x, width = 20)
}


# plotting (to see how it looks before animation)
plot <- ggplot(data = data, mapping = aes(x = Category, y = Percent, fill = Gender)) +
  geom_col(width = 0.6)  +
  scale_fill_manual(values = cols, breaks = rev(levels(data$Gender))) + 
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = plotBreak,
                     labels = plotBreakLabel,
                     expand = c(0, 0, 0, 0)) +
  scale_x_discrete(labels = autoWrap) +
  labs(x = "", y = "Percent", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
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
# gifReplicate <- function(x) {
#   grid.newpage()
#   grid.draw(x)
# }


# create GIF - version: pause in each year ----- 

saveGIF({
  print(Sys.time())
  for (i in 1:max(tf$.frame)) {
    print(paste0("working on the ", i, "th frame"))
    g <- ggplot(data = subset(tf, .frame == i), mapping = aes(x = Category, y = Percent, .frame = i)) +
      geom_col(aes(fill = Gender),
               width = 0.6)  +
      scale_fill_manual(values = cols, breaks = rev(levels(data$Gender))) + 
      coord_flip(clip = "off") +
      scale_y_continuous(breaks = plotBreak,
                         labels = plotBreakLabel,
                         expand = c(0, 0, 0, 0),
                         limits = c(0,100)) +
      scale_x_discrete(labels = autoWrap) +
      labs(x = "", y = "Percent", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
      theme_bw() +
      theme_white +
    
    #add white label (so it's invisible at earlier frames)
      geom_text(data = subset(tf, .frame == max(tf$.frame)),
                mapping = aes(x = Category, y = Percent, label = labelNormal, group = Gender),
                color = "white", size = 7,
                position = position_stack(vjust = 0.5))
       
    
    
    g <- ggplotGrob(g)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    #g$layout$l[g$layout$name == "guide-box"] <- 4
    
    # if i <= 51 (aka when i is still showing the first level "Male"), cover the legend box for "Female"
    if (i <= 51) {
      
      g2 <- ggdraw(g) + 
        geom_rect(aes(xmin = 0.58, xmax = 0.68, ymin = 0.09, ymax = 0.15),
                  colour = "white", fill = "white")
    } else {
      g2 <- ggdraw(g)
    }

    # grid::grid.draw(g);
    # grid.newpage()
    
    # define object that store the name of frame with which we want the plot to "pause"
   # pause_frames <- tf %>% group_by(df_id) %>% slice(max(row_number(.frame))) %>% pull(.frame) #this is not correct it turns out
    pause_frames <- c(51, 100)
    
   
    
    # draw the plot
    if (i %in% pause_frames){
      # replicate (and draw) the plot many times
      #grid::grid.draw(g2);
      # let the last plot pause a bit more
      if (i == pause_frames[length(pause_frames)]){
        replicate(200, grid::grid.draw(g2))
      } else {
        replicate(30, grid::grid.draw(g2))
      }
      
    } else if(i == 1){ #unfortunately, when `ggdraw` is used, the first time grid::grid.draw(g2) is run, there will be a blank page saved into the graphic device. my previous solution is to manually delete the first blank frame in Photoshop after the gif is generated. This time around 4/29/20, I think by switching back to use `g` which is not used by ggdraw is helpful
      grid::grid.draw(g); 
      } else {
      # just draw the plot one time 
      grid::grid.draw(g2)
    }
    
  }
  print(Sys.time())
},
# specify the pathway and name of the gif output, as well as the interval, width, and height
movie.name=here("Code", "COE", "Results", "CTA-6_2020_v1.gif"),interval = .02, ani.width = 1200, ani.height = 800) 


#compressing
gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

gif_compress("/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/COE/Results/CTA-6_2020_v1.gif","/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/COE/Results/CTA-6_2020_v1_compressed.gif")




