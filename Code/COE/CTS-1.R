### COE indicators data viz
### This is to be used as a gif tweet for the NCES handle
### 4/29/20 update
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
df <- read_excel(path = here("Code", "COE", "Materials", "CTS-1_2020cleanData.xlsx"),
                 sheet = "processed") %>% 
  # mutate the year column so that later ggplot understands it
  separate(col = AcademicYear, into = c("Year1", "Year2"), sep = "-", remove = FALSE) %>% 
  # keep only the needed columns
  select(-c(AcademicYear, Year2)) %>% 
  rename(Year = Year1) 


# reshape data
df <- df %>% 
  gather(key = Category, value = Value, -Year) %>% 
  # add superscripts for some categories
  mutate(Category = ifelse(Category %in% "Certificates", "Certificates^1", Category),
         Category = ifelse(Category %in% "Doctor's", "\"Doctor's\"^2", Category),
         Category = ifelse(Category %in% "Master's", "\"Master's\"", Category),
         Category = ifelse(Category %in% "Bachelor's", "\"Bachelor's\"", Category),
         Category = ifelse(Category %in% "Associate's", "\"Associate's\"", Category)) 


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

tf$nudge_y <- ifelse(tf$Category %in% "\"Associate's\"", 15000, 
                     ifelse(tf$Category %in% c("Certificates^1"),-25000, 
                            ifelse(tf$Category %in% c("\"Master's\""),-10000, 0)))
#tf$nudge_x <- ifelse(tf$Category %in% "\"Associate's\"", 5, ifelse(tf$Category %in% c("Certificates^1"),-5, 0))




# color  
# nces_palette =  c("#fbab18", "#3EC7F4", "#3FA66C","#242953")
cols <- c("#fbab18", "#3EC7F4", "#3FA66C","#242953", "#fb3a18")


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
    
# ggtext is such a life saver, so i don't have to use monster code above, which is also slowing down everything 
plotCaption <- "<span><sup>1</sup>Data are for certificates below the associate's degree level.<br>
<sup>2</sup>Includes Ph.D., Ed.D., and comparable degrees at the doctoral level. Includes most degrees formerly classified as first-professional, such as M.D., D.D.S., and law degrees.<br>
NOTE: Data are for postsecondary institutions participating in Title IV federal financial aid programs. Some data have been revised from previously published figures.<br>
SOURCE: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS), Fall 2001 through Fall 2018,<br>
Completions component. See <i style='font-family: PublicoText-Italic'>Digest of Education Statistics 2019,</i> Table 318.40.</span>"


# plotCaption <- list()
# plotNote <- expression('NOTE: Education systems are ordered by the percentage of students reaching the '~italic('Advanced')~' international benchmark.')
# plotSource <- c("\nSOURCE: International Association for the Evaluation of Educational Achievement (IEA), Progress in International Reading Literacy Study (PIRLS), 2016.")
# plotCaption <- c(plotNote, plotSource)
# plotCaption <- paste(plotCaption, collapse = "\n")
# 
# plotCaption <- getWrappedText(plotCaption, width = 1000, ps = 10)


#plotSubtitle <- "Education system"
plotSubtitle <- "Number"

plotTitle <- c("Number of certificates and degrees conferred by postsecondary institutions: \nAcademic years 2000—01 through 2017—18")
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
                     plot.subtitle=element_text(size=26, margin = margin(t=15, b = 5),family = "PublicoText-Bold", color = "black"),
                     plot.caption=element_markdown(size=15, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "PublicoText-Roman"),
                     #strip.text.x = element_text(size=18, angle = 0, hjust = .5, family = "PublicoText-Roman"),
                     #strip.background = element_rect(fill = "#f1f1f1", colour = NA),
                     legend.position="none",
                     plot.margin = margin(t = 0,r = 140, b = 0, l = 0, unit = "pt") #make enough white space to the right for geom_text labels
)



# # function to wrap x axis labels
# 
# autoWrap <- function(x) {
#   str_wrap(x, width = 20)
# }


# x axis break labels and levels
xAxisBreaks <- unique(df$Year)
xAxisLabels <- paste0(year(unique(df$Year)), "—", 
                      #last two characters of "2001", etc.
                      str_sub(year(unique(df$Year))+1,-2)) %>% 
                      # empty a few xAxisLabels values based on condition (otherwise the x axis labels gets too crowded)
                      ifelse(. %in% c("2000—01", "2005—06", "2010—11", "2015—16", "2017—18"), ., "")
  


# y axis break labels and levels
yAxisBreaks <- seq(0, 2000000, by = 200000)
yAxisLabels <- paste0(format(seq(0, 2000000, by = 200000), big.mark = ","))
# yAxisLabels <- c(yAxisLabels[1:length(yAxisLabels)-1], paste0("$",yAxisLabels[length(yAxisLabels)]))
yAxisLimits <- c(0,max(yAxisBreaks)* 1.03) 



# # plotting (to see how it looks before animation)
# wholeFig <- ggplot(tf, aes(x = Year, y = Value)) +
#   geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=10) + 
#   geom_point(data = subset(tf, .frame == max(.frame)),aes(group=Category, color=Category), size=10) +
#   geom_line(aes(group=Category, color=Category), size=2.5) +
#   scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("2000-02-25", "2016-07-21"))) +                           
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
      ##mannually adjust the limits here to make the x axis line cover the label of the first and the last year
      scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("2000-01-01", "2018-04-01"))) +                           
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + 
      scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
      labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, 
           caption = plotCaption) 
    
    # when i == 1, geom_dl wont' work
    if(i!=1){
      g <- g +
        geom_text(data = subset(tf, .frame == i), aes(label = Category), hjust=-0.2, vjust=0, show.legend = FALSE, parse=TRUE, size = 7, nudge_y = subset(tf, .frame == i)$nudge_y, family = "PublicoText-Roman") +
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
        replicate(200,gifReplicate(plot))
      } else {
        replicate(30,gifReplicate(plot)) #it doesn't apply here
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
movie.name=here("Code", "COE", "Results", "CTS-1_2020_v5.gif"),interval = .02, ani.width = 1200, ani.height = 800)

#compressing
gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

gif_compress("/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/COE/Results/CTS-1_2020_v5.gif","/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/COE/Results/CTS-1_2020_v5_compressed.gif")



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




