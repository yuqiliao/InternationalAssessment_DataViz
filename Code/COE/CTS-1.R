### COE indicators data viz
### This is to be used as a gif tweet for the NCES handle
### 5/9/19
### Yuqi Liao


### Set things up ------
# install.packages("installr")
# library(installr)
# install.ImageMagick()
# Sys.setenv(PATH = paste("C:/PROGRA~1/ImageMagick-7.0.8-Q16",
#                         Sys.getenv("PATH"), sep = ";"))
# ani.options(convert = 'C:/PROGRA~1/ImageMagick-7.0.8-Q16/convert.exe')
# library(gganimate) # gh_install_packages("dgrtwo/gganimate", ref = "26ec501") #devtools::install_github("dgrtwo/gganimate")
# library(purrr)
# library(cowplot)

# define and load all packages
reqpkg <- c("ggplot2","scales", "tidyr", "Cairo", "extrafont", "dplyr", "lubridate", "tweenr", "animation", "RColorBrewer", "grid", "gridExtra", "directlabels", "gganimate", "ggrepel", "showtext", "ggplotify", "here", "stringr", "readxl")

sapply(reqpkg, function(pkgi) {
  if (!pkgi %in% installed.packages()) {
    install.packages(pkgi, repos = "http://cran.us.r-project.org")
  }
  library(pkgi, character.only = TRUE)
})

# inspect the working directory
here()


### Read in data  -----
# the data has already been processed, based on the raw data from "tabn318.40.xls"
df <- read_excel(path = here("Code", "COE", "Materials", "CTS-1_cleanData.xlsx"),
                 sheet = "processed") %>% 
  # mutate the year column so that later ggplot understands it
  separate(col = AcademicYear, into = c("Year1", "Year2"), sep = "-", remove = FALSE) %>% 
  # keep only the needed columns
  select(-c(AcademicYear, Year2)) %>% 
  rename(Year = Year1) 


# reshape data
df <- df %>% 
  gather(key = Category, value = Value, -Year)


### Plotting ------

# make `Category` factors
df$Category <- as.factor(df$Category)

# process `Year`
df$Year <- as.Date(as.character(df$Year),"%Y")

# function to create list of data sets from the original data frame (by state)
states1<-unique((df$Year))
myf<-function(mystate){as.data.frame(df[df$Year==as.Date(mystate),])}

# use lapply to generate the list of data sets:
my.list1<-lapply(states1,myf)

# Apply tweenr: this adds in values between our data points to "smooth" the line
tf <- tween_states(my.list1, tweenlength= 1, statelength=0, ease='linear',nframes=80)
tf$Year <- as.Date(tf$Year,"%Y")

# needed to add in last 5 data points so that they match exactly the final year
# grab the first five rows as data shells
temp <- tf[tf$.frame==1,]
# replace column .frame with 81
temp$.frame <- max(tf$.frame) + 1
temp2 <- df %>% filter(Year == max(Year))
temp$Year <- NULL
temp$Category <- NULL
temp$Value <- NULL
temp <- cbind(temp2, temp)
tf <- rbind(tf, temp)






# color  
# nces_palette =  c("#fbab18", "#3EC7F4", "#3FA66C","#242953")
cols <- c("#fbab18", "#3EC7F4", "#3FA66C","#242953", "#fb3a18")


# 
# # define breaks
# plotBreak = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
# plotBreakLabel = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")


# define title/caption, etc.
plotCaption <- expression(atop('NOTE: Education systems are ordered by the percentage of students reaching the '~italic('Advanced')~' international benchmark.                                                               ','SOURCE: International Association for the Evaluation of Educational Achievement (IEA), Progress in International Reading Literacy Study (PIRLS), 2016.'))

# plotCaption <- list()
# plotNote <- expression(paste("NOTE: Education systems are ordered by the percentage of students reaching the ", italic("Advanced")," international benchmark."))
# plotSource <- c("\nSOURCE: International Association for the Evaluation of Educational Achievement (IEA), Progress in International Reading Literacy Study (PIRLS), 2016.")
# plotCaption <- c(plotNote, plotSource)
# plotCaption <- paste(plotCaption, collapse = "\n")
# 
# plotCaption <- getWrappedText(plotCaption, width = 400, ps = 10)
# 

#plotSubtitle <- "Education system"
plotSubtitle <- "Number"

plotTitle <- c("Figure CTS-1. Number of certificates and degrees conferred by postsecondary \ninstitutions: Academic years 2000-01 through 2016-17")
#plotTitle <- getWrappedText(plotTitle, width = 350, ps = 10)

# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(text = element_text(family="Calibri", color = "black"),
                     panel.grid = element_blank(), panel.border = element_blank(),
                     axis.title.x=element_text(size=26, margin = margin(t=15, b = 5), hjust = .5),
                     axis.text.x=element_text(size=22, angle = 0, hjust = 1, family = "Calibri"),
                     axis.text.y=element_text(size=22, family = "Calibri"),
                     axis.line.x=element_line(size = 1),
                     axis.line.y=element_line(size = 1),
                     axis.ticks.x = element_blank(),  
                     axis.ticks.y = element_line(size = 1),
                     plot.title=element_text(size=31,family = "Calibri", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
                     plot.subtitle=element_text(size=26, margin = margin(t=15, b = 5),family = "Calibri"),
                     plot.caption=element_text(size=16, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "Calibri"),
                     strip.text.x = element_text(size=18, angle = 0, hjust = .5, family = "Calibri Light"),
                     strip.background = element_rect(fill = "#f1f1f1", colour = NA),
                     legend.position="none"
)



# # function to wrap x axis labels
# 
# autoWrap <- function(x) {
#   str_wrap(x, width = 20)
# }


# x axis break labels and levels
xAxisBreaks <- unique(df$Year)
xAxisLabels <- paste0(year(unique(df$Year)), "-", 
                      #last two characters of "2001", etc.
                      str_sub(year(unique(df$Year))+1,-2)) %>% 
                      # empty a few xAxisLabels values based on condition (otherwise the x axis labels gets too crowded)
                      ifelse(. %in% c("2000-01", "2005-06", "2010-11", "2016-17"), ., "")
  


# y axis break labels and levels
yAxisBreaks <- seq(0, 2000000, by = 200000)
yAxisLabels <- paste0(format(seq(0, 2000000, by = 200000), big.mark = ","))
# yAxisLabels <- c(yAxisLabels[1:length(yAxisLabels)-1], paste0("$",yAxisLabels[length(yAxisLabels)]))
yAxisLimits <- c(0,max(yAxisBreaks)* 1.03)



# plotting (to see how it looks before animation)
wholeFig <- ggplot(tf, aes(x = Year, y = Value)) +
  geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=10) + 
  geom_point(data = subset(tf, .frame == max(.frame)),aes(group=Category, color=Category), size=10) +
  geom_line(aes(group=Category, color=Category), size=2.5) +
  scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("2000-02-25", "2016-07-21"))) +                           
  scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
  # geom_text(data = subset(tf, .frame == min(.frame)), size = 9, 
  #           aes(label = round(Value, 0)),
  #           nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
  #           nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
  #           family="Calibri") + 
  # geom_text(data = subset(tf, .frame == max(.frame)), size = 9, 
  #           aes(label = round(Value, 0)),
  #           nudge_y = subset(tf, .frame == max(.frame))$nudge_y, 
  #           nudge_x= subset(tf, .frame == max(.frame))$nudge_x, 
  #           family="Calibri") +
  theme_minimal() + theme_white + 
  scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
  labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, 
       caption = plotCaption) +
  geom_dl(data = tf,
          aes(label = Category), method = list("last.points", cex = 2, hjust = -0.5))











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

# version 1 - pause in each year (and draw dots)
saveGIF({
  print(Sys.time())
  for (i in 1:max(tf$.frame)) {
    print(paste0("working on the ", i, "th frame"))
    g <- ggplot(data = subset(tf, .frame <= i), aes(x = Year, y = Value, .frame = i)) +
      geom_line(aes(group=Category, color=Category), size=2.5) +
      scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("2000-02-25", "2018-10-21"))) +                           
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + 
      scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
      labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, 
           caption = plotCaption) 
    
    # when i == 1, geom_dl wont' work
    if(i!=1){
      g <- g +
        geom_dl(data = subset(tf, .frame <= i),
                aes(label = Category), method = list("last.qp", cex = 2, hjust = -0.2))
    }
    
    
    # add geom_point to g based on i
    X_axis_years <- unique(df$Year)
    Current_years <- unique(subset(tf, .frame <= i)$Year)
    a <- X_axis_years %in% Current_years
    
    g <- g + geom_point(data = subset(tf, Year %in% X_axis_years[a]),aes(group=Category, color=Category), size=6)
      # # add geom_text to g based on i
      # geom_text(data = subset(tf, Year %in% X_axis_years[a]), size = 9, 
      #           aes(label = round(Value, 0)),
      #           nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
      #           nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
      #           family="Calibri")
    
    #grob
    plot <- ggplotGrob(g)
    
    # define object that store the name of frame with which we want the plot to "pause"
    pause_frames <- unique(tf[tf$Year %in% X_axis_years , ]$.frame)
    
    
    
    
    # draw the plot
    if (i %in% pause_frames){
      # replicate (and draw) the plot many times
      grid::grid.draw(plot);
      # let the last plot pause a bit more
      if (i == pause_frames[length(pause_frames)]){
        replicate(200,gifReplicate(plot))
      } else {
        replicate(10,gifReplicate(plot))
      }
      
    } else {
      # just draw the plot one time
      grid::grid.draw(plot);
      grid.newpage()
    }
    
    
  }  
  print(Sys.time())
},
# specify the pathway and name of the gif output, as well as the interval, width, and height
movie.name=here("Code", "COE", "Results", "CTS-1_v1.gif"),interval = .02, ani.width = 1200, ani.height = 1000)


# version 2 - pause only at begining and end
saveGIF({
  print(Sys.time())
  for (i in 1:max(tf$.frame)) {
    print(paste0("working on the ", i, "th frame"))
    g <- ggplot(data = subset(tf, .frame <= i), aes(x = Year, y = Value, .frame = i)) +
      geom_line(aes(group=Category, color=Category), size=2.5) +
      scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("2000-02-25", "2018-10-21"))) +                           
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + 
      scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
      labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, 
           caption = plotCaption) 
    
    # when i == 1, geom_dl wont' work
    if(i!=1){
      g <- g +
        geom_dl(data = subset(tf, .frame <= i),
                aes(label = Category), method = list("last.qp", cex = 2, hjust = -0.2))
    }
    
    
    # add geom_point to g based on i
    X_axis_years <- unique(df$Year)
    Current_years <- unique(subset(tf, .frame <= i)$Year)
    a <- X_axis_years %in% Current_years
    
    # when i == 1, add the geom_point
    if(i==1){
      g <- g + geom_point(data = subset(tf, Year %in% X_axis_years[a]),aes(group=Category, color=Category), size=6)
      
    }
    # # add geom_text to g based on i
    # geom_text(data = subset(tf, Year %in% X_axis_years[a]), size = 9, 
    #           aes(label = round(Value, 0)),
    #           nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
    #           nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
    #           family="Calibri")
    
    #grob
    plot <- ggplotGrob(g)
    
    # define object that store the name of frame with which we want the plot to "pause" (in this case, only pause at the beginning and the end)
    pause_frames <- c(1,81)
    
    
    
    
    # draw the plot
    if (i %in% pause_frames){
      # replicate (and draw) the plot many times
      grid::grid.draw(plot);
      # let the last plot pause a bit more
      if (i == pause_frames[length(pause_frames)]){
        replicate(200,gifReplicate(plot))
      } else {
        replicate(30,gifReplicate(plot))
      }
      
    } else {
      # just draw the plot one time
      grid::grid.draw(plot);
      grid.newpage()
    }
    
    
  }  
  print(Sys.time())
},
# specify the pathway and name of the gif output, as well as the interval, width, and height
movie.name=here("Code", "COE", "Results", "CTS-1_v2.gif"),interval = .02, ani.width = 1200, ani.height = 1000)







