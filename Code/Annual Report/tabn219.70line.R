### Annual report indicators data viz
### This is to be used as a gif tweet for the NCES handle
### 10/21/19
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


### Read in data  -----
# the data has already been processed
df <- read_excel(path = here("Code", "Annual Report", "Materials", "tabn219.70line_clean.xlsx"),
                 sheet = "processed") 

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
tf <- tween_states(my.list1, tweenlength= 1, statelength=0, ease='linear',nframes=400)
tf$Year <- as.Date(tf$Year,"%Y")

# needed to add in last 3 data points so that they match exactly the final year
# grab the first 3 rows as data shells
temp <- tf[tf$.frame==1,]
# replace column .frame with 161
temp$.frame <- max(tf$.frame) + 1
temp2 <- df %>% filter(Year == max(Year))
temp$Year <- NULL
temp$Category <- NULL
temp$Value <- NULL
temp <- cbind(temp2, temp)
tf <- rbind(tf, temp)

tf$nudge_y <- ifelse(tf$Category %in% "White", -2,
                     ifelse(tf$Category %in% c("Black"),0, 0))
tf$nudge_x <- 150




# color  
# nces_palette =  c("#fbab18", "#3EC7F4", "#3FA66C","#242953")
# cols <- c("#fbab18", "#3EC7F4", "#3FA66C","#242953", "#fb3a18")
cols <- c( "#3EC7F4", "#3FA66C", "#242953")

# 
# # define breaks
# plotBreak = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
# plotBreakLabel = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")


# define title/caption, etc.
#plotCaption <- expression(atop('SOURCE: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS),',"Fall 2001 through Fall 2017, Completions component. See,"~italic("Digest of Education Statistics 2018")~', Table 318.40.                                                        '))

#plotCaption <- expression(paste(''^{14}, "C", sep = ""))
#plotCaption <- expression(textstyle(paste(''^{14}, "C", sep = "")))


# plotCaption <- expression(
#   #line 1
#   atop(textstyle(paste(''^1,"Data are for certificates below the associate's degree level.                                                                                                                                                                                                      ", sep = "")),
#        #line 2
#        atop(textstyle(paste(''^2,"Includes Ph.D., Ed.D., and comparable degrees at the doctoral level. Includes most degrees formerly classified as first-professional, such as M.D., D.D.S., and law degrees.", sep = "")),
#             #line 3 (use scriptscriptstyle to add space between 2 & 3)
#             atop(scriptscriptstyle(""), atop(textstyle("SOURCE: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS), Fall 2001 through Fall 2017,     "),
#                 #line 4 (use scriptscriptstyle to add space between 3 & 4)
#                 atop(scriptscriptstyle(""), textstyle("Completions component. See"~italic("Digest of Education Statistics 2018,")~"Table 318.40.                                                                                                                                                                  "))
#                    )
#                 )
#             )
#   )
# )

plotCaption <- expression("SOURCE: U.S. Department of Commerce, Census Bureau, Current Population Survey (CPS), October 1977 through 2017.")
            

# plotCaption <- list()
# plotNote <- expression('NOTE: Education systems are ordered by the percentage of students reaching the '~italic('Advanced')~' international benchmark.')
# plotSource <- c("\nSOURCE: International Association for the Evaluation of Educational Achievement (IEA), Progress in International Reading Literacy Study (PIRLS), 2016.")
# plotCaption <- c(plotNote, plotSource)
# plotCaption <- paste(plotCaption, collapse = "\n")
# 
# plotCaption <- getWrappedText(plotCaption, width = 1000, ps = 10)


#plotSubtitle <- "Education system"
plotSubtitle <- "Percent"

plotTitle <- c("Status dropout rates of 16- to 24-year-olds, by race/ethnicity:\n1977 through 2017")
#plotTitle <- getWrappedText(plotTitle, width = 350, ps = 10)

# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(text = element_text(family="Gotham-Book", color = "black"),
                     panel.grid = element_blank(), panel.border = element_blank(),
                     axis.title.x=element_text(size=26, margin = margin(t=15, b = 5), hjust = .5),
                     axis.text.x=element_text(size=20, angle = 0, hjust = 0.3, family = "Gotham-Light"),
                     axis.text.y=element_text(size=20, family = "Gotham-Light"),
                     #axis.line.x=element_line(size = 1),
                     #axis.line.y=element_line(size = 1),
                     axis.ticks.x = element_blank(),  
                     axis.ticks.y = element_blank(),
                     plot.title=element_text(size=30,family = "Gotham-Bold", hjust= 0,lineheight=1, margin = margin(t = 15)),
                     plot.subtitle=element_text(size=26, margin = margin(t=15, b = 5),family = "Gotham-Book", face = "plain"),
                     plot.caption=element_text(size=16, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "Gotham-Book"),
                     #strip.text.x = element_text(size=18, angle = 0, hjust = .5, family = "Gotham-Light"),
                     #strip.background = element_rect(fill = "#f1f1f1", colour = NA),
                     legend.position="none"
)



# # function to wrap x axis labels
# 
# autoWrap <- function(x) {
#   str_wrap(x, width = 20)
# }


# x axis break labels and levels
xAxisBreaks <- unique(df$Year)
xAxisLabels <- year(unique(df$Year)) %>% 
                ifelse(. %in% c("1977", "1980", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2017"), ., "")
  

# y axis break labels and levels
yAxisBreaks <- seq(0, 50, by = 10)
yAxisLabels <- yAxisBreaks %>% ifelse(. %in% c("50"), "50%", .)
  
#yAxisLabels <- paste0(format(seq(0, 2000000, by = 200000), big.mark = ","))
# yAxisLabels <- c(yAxisLabels[1:length(yAxisLabels)-1], paste0("$",yAxisLabels[length(yAxisLabels)]))
yAxisLimits <- c(0,max(yAxisBreaks)* 1.03)



# # plotting (to see how it looks before animation)
# wholeFig <- ggplot(tf, aes(x = Year, y = Value)) +
#   geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=10) +
#   geom_point(data = subset(tf, .frame == max(.frame)),aes(group=Category, color=Category), size=10) +
#   geom_line(aes(group=Category, color=Category), size=2.5) +
#   scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("1977-10-22", "2017-10-22"))) +
#   scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
#   # geom_text(data = subset(tf, .frame == min(.frame)), size = 9,
#   #           aes(label = round(Value, 0)),
#   #           nudge_y = subset(tf, .frame == min(.frame))$nudge_y,
#   #           nudge_x= subset(tf, .frame == min(.frame))$nudge_x,
#   #           family="Gotham-Book") +
#   # geom_text(data = subset(tf, .frame == max(.frame)), size = 9,
#   #           aes(label = round(Value, 0)),
#   #           nudge_y = subset(tf, .frame == max(.frame))$nudge_y,
#   #           nudge_x= subset(tf, .frame == max(.frame))$nudge_x,
#   #           family="Gotham-Book") +
#   theme_minimal() + theme_white +
#   scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
#   labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, caption = plotCaption) +
#   geom_dl(data = tf,
#           aes(label = Category), method = list("last.points", cex = 2, hjust = -0.5))
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
      scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("1977-10-22", "2021-10-22")))+   #extending the end x year to make room for the text label                          
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 1), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + 
      scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
      labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, 
           caption = plotCaption) 
    
    # when i == 1, geom_dl wont' work
    if(i!=1){
      g <- g +
        geom_text(data = subset(tf, .frame == i), aes(label = Category), hjust=0, vjust=0, show.legend = FALSE, parse=FALSE, size = 7, nudge_y = subset(tf, .frame == i)$nudge_y, nudge_x = subset(tf, .frame == i)$nudge_x)
    }                     
    
    
    # # add geom_point to g based on i
    X_axis_years <- unique(df$Year)
    # Current_years <- unique(subset(tf, .frame <= i)$Year)
    # a <- X_axis_years %in% Current_years
    
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
    #           family="Gotham-Book")
    
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
        replicate(200,gifReplicate(plot))
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
movie.name=here("Code", "Annual Report", "Results", "tabn219.70line.gif"),interval = .02, ani.width = 1200, ani.height = 800)

gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

gif_compress("/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/Annual\\ Report/Results/tabn219.70line.gif","/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/Annual\\ Report/Results/tabn219.70line_compressed.gif")


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
#     #           family="Gotham-Book")
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





