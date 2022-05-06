### COE indicators data viz - line chart
### This is to be used as a gif tweet for the NCES handle
### 5/6/22 for 2021 COE efforts
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
library(ggtext)

# define and load all packages
reqpkg <- c("ggplot2","scales", "tidyr", "Cairo", "extrafont", "dplyr", "lubridate", "tweenr", "animation", "RColorBrewer", "grid", "gridExtra", "directlabels", "gganimate", "ggrepel", "showtext", "here", "stringr", "readxl")

sapply(reqpkg, function(pkgi) {
  if (!pkgi %in% installed.packages()) {
    install.packages(pkgi, repos = "http://cran.us.r-project.org")
  }
  library(pkgi, character.only = TRUE)
})

# inspect the working directory
here()

#load font
#font_import() #run only once
#loadfonts() #didn't load "Gotham" font sucessfully, will stick with "PublicoText-Roman" for now


### Read in data  -----
# the data has already been processed, based on the raw data from "tabn318.40.xls"
df <- read_excel(path = here("Code", "COE", "Materials", "CNJ-4_cleanData.xlsx"),
                 sheet = "processed")


# reshape data
df <- df %>% 
  gather(key = Category, value = Value, -Year)
  # # add superscripts for some categories
  # mutate(Category = ifelse(Category %in% "Business", "Business^1", Category),
  #        Category = ifelse(Category %in% "Health professions and related programs", "Health professions<br>and related programs", Category),
  #        Category = ifelse(Category %in% "Social sciences and history", "Social sciences<br>and history", Category),
  #        Category = ifelse(Category %in% "Biological and biomedical sciences", "Biological and<br>biomedical sciences", Category)) %>% 
  # #creat nudge_y here (instead of later) so nudge_y could be transition from 0 to the desired value 
  # mutate(nudge_y = ifelse(Year %in% "2018–19" & Category %in% "Social sciences<br>and history", 15000, 
  #                        ifelse(Year %in% "2018–19" & Category %in% "Engineering", 13000, 
  #                               ifelse(Year %in% "2018–19" & Category %in% "Biological and<br>biomedical sciences", -8000, 
  #                                      ifelse(Year %in% "2018–19" & Category %in% "Psychology", -30000, 0)))))

### Plotting ------

# make `Category` factors
df$Category <- as.factor(df$Category)

# process `Year`
df$Year <- as.Date(as.character(df$Year),"%Y")
# lastYear <- as.Date(as.character(df$Year[length(df$Year)]),"%Y") #this could dynamically identify the last year

# function to create list of data sets from the original data frame (by state)
states1<-unique((df$Year))
myf<-function(mystate){as.data.frame(df[df$Year==as.Date(mystate),])}

# use lapply to generate the list of data sets:
my.list1<-lapply(states1,myf)

# Apply tweenr: this adds in values between our data points to "smooth" the line
tf <- tween_states(my.list1, tweenlength= 1, statelength=0, ease='linear',nframes=160)
tf$Year <- as.Date(tf$Year,"%Y")

# needed to add in the last set of data points so that they match exactly the final year
# grab the first set of rows as data shells
temp <- tf[tf$.frame==1,]
# replace column .frame with 81
temp$.frame <- max(tf$.frame) + 1
temp2 <- df %>% filter(Year == max(Year))
temp$Year <- NULL
temp$Category <- NULL
temp$Value <- NULL
temp$nudge_y <- NULL
temp <- cbind(temp2, temp)
tf <- rbind(tf, temp)

# tf$nudge_y <- ifelse(tf$Category %in% "Social sciences<br>and history", 15000,
#                      ifelse(tf$Category %in% "Engineering",13000,
#                             ifelse(tf$Category %in% "Biological and<br>biomedical sciences",-8000,
#                                    ifelse(tf$Category %in% "Psychology",-30000,0))))
#tf$nudge_x <- ifelse(tf$Category %in% "\"Associate's\"", 5, ifelse(tf$Category %in% c("Certificates^1"),-5, 0))




# color  
# nces_palette =  c("#fbab18", "#3EC7F4", "#3FA66C","#242953")
cols <- c("#fbab18", "#3EC7F4", "#3FA66C","#242953", "#971b2f", "#84329B")

plotCaption <- "<span><sup>1</sup> &ldquo;Business&rdquo; is defined as business, management, marketing, and related support services, as well as personal and culinary services.<br>
SOURCE: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS),<br>Fall 2010 through Fall 2019, Completions component. See <i style='font-family: PublicoText-Italic'>Digest of Education Statistics 2020,</i> table 322.10.</span>"

plotSubtitle <- "Number of degrees"

plotTitle <- c("Number of bachelor’s degrees conferred by postsecondary institutions\nin selected fields of study: 2009–10 through 2018–19")
#plotTitle <- getWrappedText(plotTitle, width = 350, ps = 10)

# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(text = element_text(family="PublicoText-Roman", color = "black"),
                     panel.grid = element_blank(),
                     panel.grid.major.y = element_line(size = 0.25, color = "#576F7F", linetype = "solid"), 
                     panel.border = element_blank(),
                     axis.title.x=element_text(size=26, margin = margin(t=15, b = 5), hjust = .5, family = "PublicoText-Bold", color = "black"),
                     axis.text.x=element_text(size=22, angle = 0, hjust = 0.3, vjust = -1, family = "PublicoText-Roman"),
                     axis.text.y=element_text(size=22, family = "PublicoText-Roman"),
                     axis.line.x=element_line(size = 0.25, color = "#576F7F"),
                     #axis.line.y=element_line(size = 1),
                     axis.ticks.x = element_blank(),  
                     axis.ticks.y = element_blank(),
                     axis.ticks.length.y = unit(0.5, "cm"),
                     plot.title=element_text(size=30,family = "PublicoText-Bold", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
                     plot.subtitle=element_text(size=26, margin = margin(t=25, b = 20),family = "PublicoText-Bold", color = "black"),
                     plot.caption=element_markdown(size=15, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "PublicoText-Roman"),
                     #strip.text.x = element_text(size=18, angle = 0, hjust = .5, family = "PublicoText-Roman"),
                     #strip.background = element_rect(fill = "#f1f1f1", colour = NA),
                     legend.position="none",
                     plot.margin = margin(t = 0,r = 200, b = 0, l = 0, unit = "pt") #make enough white space to the right for geom_text labels
)



# # function to wrap x axis labels
# 
# autoWrap <- function(x) {
#   str_wrap(x, width = 20)
# }


# x axis break labels and levels
xAxisBreaks <- unique(df$Year)
xAxisLabels <- year(unique(df$Year))


# y axis break labels and levels
yAxisBreaks <- seq(175, 350, by = 25)
yAxisLabels <- paste0(format(seq(175, 350, by = 25), big.mark = ","))
#yAxisLabels <- paste0("$",yAxisLabels)
#yAxisLabels <- c(yAxisLabels[1:length(yAxisLabels)-1], paste0("$",yAxisLabels[length(yAxisLabels)]))
yAxisLimits <- c(175,max(yAxisBreaks)* 1.03) 



# # plotting (to see how it looks before animation)
# wholeFig <- ggplot(tf, aes(x = Year, y = Value)) +
#   geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=10) +
#   geom_point(data = subset(tf, .frame == max(.frame)),aes(group=Category, color=Category), size=10) +
#   geom_line(aes(group=Category, color=Category), size=2.5) +
#   scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("1973-02-25", "2020-07-21"))) +
#   scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
#   # geom_text(data = subset(tf, .frame == min(.frame)), size = 9,
#   #           aes(label = round(Value, 0)),
#   #           nudge_y = subset(tf, .frame == min(.frame))$nudge_y,
#   #           nudge_x= subset(tf, .frame == min(.frame))$nudge_x,
#   #           family="PublicoText-Roman") +
#   # geom_text(data = subset(tf, .frame == max(.frame)), size = 9,
#   #           aes(label = round(Value, 0)),
#   #           nudge_y = subset(tf, .frame == max(.frame))$nudge_y,
#   #           nudge_x= subset(tf, .frame == max(.frame))$nudge_x,
#   #           family="PublicoText-Roman") +
#   theme_minimal() + theme_white +
#   scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
#   labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
#   geom_dl(data = tf,
#           aes(label = Category), method = list("last.points", cex = 2, hjust = 0)) +
#   coord_cartesian(clip = 'off')
# 
# wholeFig
# 
# 
# # the below code to change plot layout (for better alignment) won't work with gganimate, so give up the gganimate for now
# grid.newpage()
# # ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
# g <- ggplotGrob(wholeFig)
# g$layout$l[g$layout$name == "title"] <- 4
# g$layout$l[g$layout$name == "caption"] <- 4
# g$layout$l[g$layout$name == "subtitle"] <- 4
# #g$layout$l[g$layout$name == "guide-box"] <- 4
# grid::grid.draw(g);





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

# version 2 - pause only at begining and end =====
saveGIF({
  print(Sys.time())
  for (i in 1:max(tf$.frame)) {
    print(paste0("working on the ", i, "th frame"))
    g <- ggplot(data = subset(tf, .frame <= i), aes(x = Year, y = Value, .frame = i)) +
      geom_line(aes(group=Category, color=Category), size=2.5) +
      ##mannually adjust the limits here to make the x axis line cover the label of the first and the last year
      scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("1973-02-25", "2020-07-21"))) +
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + 
      scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
      labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, 
           caption = plotCaption) 
    
    # when i == 1, geom_dl wont' work
    if(i!=1){
      g <- g +
        geom_richtext(data = subset(tf, .frame == i), aes(label = Category, color = Category), lineheight = 0.5, hjust=0, vjust=0.5, show.legend = FALSE, size = 7, nudge_x = 40, nudge_y = subset(tf, .frame == i)$nudge_y, family = "PublicoText-Roman", fill = NA, label.color = NA) +
        coord_cartesian(clip = 'off') #to avoid the label being cut off in the margin area
    }
    
    # add geom_point to g based on i
    X_axis_years <- unique(df$Year)
    Current_years <- unique(subset(tf, .frame <= i)$Year)
    a <- X_axis_years %in% Current_years
    
    # add the geom_point for the first year (for all years)
    g <- g + geom_point(data = subset(tf, Year %in% X_axis_years[1]),aes(group=Category, color=Category), size=6)
    
    # add the geom_point for the last year (if i is the last frame)
    if(i == max(tf$.frame)) {
      g <- g + geom_point(data = subset(tf, Year %in% X_axis_years[length(X_axis_years)]),aes(group=Category, color=Category), size=6)
    }
    
    # # add geom_text to g based on i
    # geom_text(data = subset(tf, Year %in% X_axis_years[a]), size = 9, 
    #           aes(label = round(Value, 0)),
    #           nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
    #           nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
    #           family="PublicoText-Roman")
    
    #grob
    # ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
    plot <- ggplotGrob(g)
    plot$layout$l[plot$layout$name == "title"] <- 4
    plot$layout$l[plot$layout$name == "caption"] <- 4
    plot$layout$l[plot$layout$name == "subtitle"] <- 4
    #plot$layout$l[plot$layout$name == "guide-box"] <- 4

    
    
    
    # define object that store the name of frame with which we want the plot to "pause" (in this case, only pause at the beginning and the end)
    pause_frames <- c(max(tf$.frame))
    
    
    
    
    # draw the plot
    if (i %in% pause_frames){
      # replicate (and draw) the plot many times
      grid::grid.draw(plot);
      # let the last plot pause a bit more
      if (i == pause_frames){
        replicate(2,gifReplicate(plot))
      } else {
        replicate(3,gifReplicate(plot)) #it doesn't apply here
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
movie.name=here("Code", "COE", "Results", "CNJ-4_v1.gif"),interval = .02, ani.width = 1200, ani.height = 800)

#compressing
gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

gif_compress("/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/COE/Results/CTA-4_v4.gif","/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/COE/Results/CTA-4_v4_compressed.gif")



#  # version 1 - pause in each year (and draw dots) - not used =====
# saveGIF({
#   print(Sys.time())
#   for (i in 1:max(tf$.frame)) {
#     print(paste0("working on the ", i, "th frame"))
#     g <- ggplot(data = subset(tf, .frame <= i), aes(x = Year, y = Value, .frame = i)) +
#       geom_line(aes(group=Category, color=Category), size=2.5) +
#       scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("2000-02-25", "2018-10-21"))) +                           
#       scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
#       theme_minimal() + theme_white + 
#       scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
#       labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, 
#            caption = plotCaption) 
#     
#     # when i == 1, geom_dl wont' work
#     if(i!=1){
#       g <- g +
#         geom_dl(data = subset(tf, .frame <= i),
#                 aes(label = Category), method = list("last.qp", cex = 2, hjust = -0.2))
#     }
#     
#     
#     # add geom_point to g based on i
#     X_axis_years <- unique(df$Year)
#     Current_years <- unique(subset(tf, .frame <= i)$Year)
#     a <- X_axis_years %in% Current_years
#     
#     g <- g + geom_point(data = subset(tf, Year %in% X_axis_years[a]),aes(group=Category, color=Category), size=6)
#     # # add geom_text to g based on i
#     # geom_text(data = subset(tf, Year %in% X_axis_years[a]), size = 9, 
#     #           aes(label = round(Value, 0)),
#     #           nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
#     #           nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
#     #           family="PublicoText-Roman")
#     
#     #grob
#     plot <- ggplotGrob(g)
#     
#     # define object that store the name of frame with which we want the plot to "pause"
#     pause_frames <- unique(tf[tf$Year %in% X_axis_years , ]$.frame)
#     
#     
#     
#     
#     # draw the plot
#     if (i %in% pause_frames){
#       # replicate (and draw) the plot many times
#       grid::grid.draw(plot);
#       # let the last plot pause a bit more
#       if (i == pause_frames[length(pause_frames)]){
#         replicate(200,gifReplicate(plot))
#       } else {
#         replicate(10,gifReplicate(plot))
#       }
#       
#     } else {
#       # just draw the plot one time
#       grid::grid.draw(plot);
#       grid.newpage()
#     }
#     
#     
#   }  
#   print(Sys.time())
# },
# # specify the pathway and name of the gif output, as well as the interval, width, and height
# movie.name=here("Code", "COE", "Results", "CTS-1_v1.gif"),interval = .02, ani.width = 1200, ani.height = 1000)
# 



