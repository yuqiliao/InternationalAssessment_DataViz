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

# df_original <- read_excel(path = here("Code", "COE", "Materials", "CNJ-4_cleanData.xlsx"),
#                  sheet = "processed_original")
# df_revised<- read_excel(path = here("Code", "COE", "Materials", "CNJ-4_cleanData.xlsx"),
#                           sheet = "processed_revised")

# reshape data
df <- df %>% 
  pivot_longer(cols = c(White, Black, Hispanic, White_original, Black_original, Hispanic_original),names_to = "Category") %>% 
  rename(Value = value)

# 
# 
# df_original <- df_original %>% 
#   gather(key = Category, value = Value, -Year)
# 
# df_revised <- df_revised %>% 
#   gather(key = Category, value = Value, -Year)
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
# df <- df_original
# make `Category` factors
df$Category <- as.factor(df$Category)

# process `Year`
df$Year <- as.Date(paste0(as.character(df$Year),"/05/01"),"%Y/%m/%d")
# lastYear <- as.Date(as.character(df$Year[length(df$Year)]),"%Y") #this could dynamically identify the last year

# function to create list of data sets from the original data frame (by state)
states1<-unique((df$Order))



myf<-function(mystate){as.data.frame(df[df$Order==mystate,])}

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
temp2 <- df %>% filter(Order == max(Order))
temp$Year <- NULL
temp$Order <- NULL
temp$Category <- NULL
temp$Value <- NULL
#temp$nudge_y <- NULL
temp <- cbind(temp2, temp)
tf <- rbind(tf, temp)


## because of the strange divider at year 2004, 2004's data could also be considered as the "last year data" and thus to be accurate, i need to mannually add back the three data points for the orginal assessment groups
temp3 <- df %>% filter(Order == 27)
tf[tf$Order==27 & tf$Category == "White_original", "Value"] <- temp3[temp3$Category=="White_original", "Value"]
tf[tf$Order==27 & tf$Category == "Black_original", "Value"] <- temp3[temp3$Category=="Black_original", "Value"]
tf[tf$Order==27 & tf$Category == "Hispanic_original", "Value"] <- temp3[temp3$Category=="Hispanic_original", "Value"]

# check, now that the following query should return no NA under the "Value" column
tf[tf$Order==27,]
# 
# # make `Category` factors
# df_revised$Category <- as.factor(df_revised$Category)
# 
# # process `Year`
# df_revised$Year <- as.Date(as.character(df_revised$Year),"%Y")
# # lastYear <- as.Date(as.character(df$Year[length(df$Year)]),"%Y") #this could dynamically identify the last year
# 
# # function to create list of data sets from the original data frame (by state)
# states1<-unique((df_revised$Year))
# myf<-function(mystate){as.data.frame(df_revised[df_revised$Year==as.Date(mystate),])}
# 
# # use lapply to generate the list of data sets:
# my.list1<-lapply(states1,myf)
# 
# # Apply tweenr: this adds in values between our data points to "smooth" the line
# tf2 <- tween_states(my.list1, tweenlength= 1, statelength=0, ease='linear',nframes=68)
# tf2$Year <- as.Date(tf$Year,"%Y")
# 
# # needed to add in the last set of data points so that they match exactly the final year
# # grab the first set of rows as data shells
# temp <- tf2[tf2$.frame==1,]
# # replace column .frame with 81
# temp$.frame <- max(tf2$.frame) + 1
# temp2 <- df_revised %>% filter(Year == max(Year))
# temp$Year <- NULL
# temp$Category <- NULL
# temp$Value <- NULL
# temp$nudge_y <- NULL
# temp <- cbind(temp2, temp)
# tf2 <- rbind(tf2, temp)








# tf$nudge_y <- ifelse(tf$Category %in% "Social sciences<br>and history", 15000,
#                      ifelse(tf$Category %in% "Engineering",13000,
#                             ifelse(tf$Category %in% "Biological and<br>biomedical sciences",-8000,
#                                    ifelse(tf$Category %in% "Psychology",-30000,0))))
#tf$nudge_x <- ifelse(tf$Category %in% "\"Associate's\"", 5, ifelse(tf$Category %in% c("Certificates^1"),-5, 0))




# color  
# nces_palette =  c("#fbab18", "#3EC7F4", "#3FA66C","#242953")
cols <- c("#fbab18", "#fbab18", "#3EC7F4","#3EC7F4", "#3FA66C", "#3FA66C")
#cols <- c("#fbab18", "#3EC7F4", "#3FA66C")


plotCaption <- "<span>NOTE: NAEP scores range from 0 to 500. Several changes were made to the long-term trend assessment in 2004 to align it with current assessment practices and<br>policies applicable to the NAEP main assessments. This included allowing accommodations for students with disabilities and for English learners. These changes have<br>been carried forward in more recent data collections. Race categories exclude persons of Hispanic ethnicity.<br>
SOURCE: U.S. Department of Education, National Center for Education Statistics, National Assessment of Educational Progress (NAEP), <i style='font-family: PublicoText-Italic'>NAEP 2020 Trends in Academic<br>Progress</i>; and 2020 NAEP Long-Term Trend Mathematics Assessment. See <i style='font-family: PublicoText-Italic'>Digest of Education Statistics 2021</i>, table 222.85.</span>"
 
# plotCaption <- "<span><sup>1</sup> &ldquo;Business&rdquo; is defined as business, management, marketing, and related support services, as well as personal and culinary services.<br>
# SOURCE: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS),<br>Fall 2010 through Fall 2019, Completions component. See <i style='font-family: PublicoText-Italic'>Digest of Education Statistics 2020,</i> table 322.10.</span>"

plotSubtitle <- "Score"
plotTitle <- c("Average mathematics scale scores on the long-term trend\nNational Assessment of Educational Progress (NAEP) for 13-year-olds,\nby race/ethnicity: Selected years, 1978 through 2020")

#plotTitle <- getWrappedText(plotTitle, width = 350, ps = 10)

# NCES theme, which gets slightly adjusted for each visualization
theme_white <- theme(text = element_text(family="PublicoText-Roman", color = "black"),
                     panel.grid = element_blank(),
                     panel.grid.major.y = element_line(size = 0.25, color = "#576F7F", linetype = "solid"), 
                     panel.border = element_blank(),
                     axis.title.x=element_text(size=26, margin = margin(t=15, b = 5), hjust = .5, family = "PublicoText-Bold", color = "black"),
                     axis.text.x=element_text(size=19, angle = 0, hjust = 0.5, vjust = -1, family = "PublicoText-Roman", margin = margin(b=10)),
                     axis.text.y=element_text(size=22, family = "PublicoText-Roman"),
                     axis.line.x=element_line(size = 0.25, color = "#576F7F"),
                     axis.line.y=element_line(size = 0.25, color = "#576F7F"),
                     axis.ticks.x = element_line(size = 1, color = "#576F7F"),  
                     axis.ticks.length.x = unit(0.25, "cm"),
                     axis.ticks.y = element_blank(),
                     axis.ticks.length.y = unit(0.5, "cm"),
                     plot.title=element_text(size=30,family = "PublicoText-Bold", face = "bold" , hjust= 0,lineheight=1, margin = margin(t = 15)),
                     plot.subtitle=element_text(size=26, margin = margin(t=25, b = 20),family = "PublicoText-Bold", color = "black"),
                     plot.caption=element_markdown(size=15, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "PublicoText-Roman"),
                     #strip.text.x = element_text(size=18, angle = 0, hjust = .5, family = "PublicoText-Roman"),
                     #strip.background = element_rect(fill = "#f1f1f1", colour = NA),
                     legend.position="none",
                     plot.margin = margin(t = 0,r = 100, b = 0, l = 0, unit = "pt") #make enough white space to the right for geom_text labels
)



# # function to wrap x axis labels
# 
# autoWrap <- function(x) {
#   str_wrap(x, width = 20)
# }


# x axis break labels and levels
xAxisYears <- c("1978", "1982", "1986", "1990", "1992", "1994", "1996", "1999", "2004", "2008", "2012","2020")

xAxisBreaks <- as.Date(paste0(xAxisYears,"/05/01"),"%Y/%m/%d")
xAxisLabels <- year(xAxisBreaks)


# y axis break labels and levels
yAxisBreaks <- seq(175, 350, by = 25)
yAxisLabels <- c(0, paste0(format(seq(175, 350, by = 25), big.mark = ","))[2:7] ,500) 
#yAxisLabels <- paste0("$",yAxisLabels)
#yAxisLabels <- c(yAxisLabels[1:length(yAxisLabels)-1], paste0("$",yAxisLabels[length(yAxisLabels)]))
yAxisLimits <- c(175,max(yAxisBreaks)) 



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
      #add the vertical line to divide original and the revised sections
      geom_vline(xintercept = date("2004-05-01"), linetype="solid", color = "#576F7F", size=1) +
      ##mannually adjust the limits here to make the x axis line cover the label of the first and the last year
      scale_x_date(labels=xAxisLabels, expand = c(0.01, 0), breaks=xAxisBreaks,limits =as.Date(c("1977-08-01", "2020-07-21"))) +
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
      theme_minimal() + theme_white + 
      scale_color_manual(values=cols) + scale_fill_manual(values=cols) +
      labs(x="Year", y="", title = plotTitle, subtitle = plotSubtitle, 
           caption = plotCaption) 
    
    
    
    
    
    # because when i==105 there are six data points, and I don't have to display all six labels, i just needed three
    if(i!=105){
      g <- g +
        # note the special treatment of <data = subset(tf, .frame == i & !is.na(Value)) , aes(label = c("White", "Black", "Hispanic")> to avoid displaying "White_orginal" and so on.
        geom_richtext(data = subset(tf, .frame == i & !is.na(Value)) , aes(label = c("White", "Black", "Hispanic"), color = Category), lineheight = 0.5, hjust=0, vjust=0.5, show.legend = FALSE, size = 7, nudge_x = 40, nudge_y = subset(tf, .frame == i)$nudge_y, family = "PublicoText-Roman", fill = NA, label.color = NA) +
        coord_cartesian(clip = 'off') #to avoid the label being cut off in the margin area
    }
    
    
    #when i == 105, it means year == "2004-05-01", where the vertical dividing line is
    g <- g +
      geom_richtext(data = data.frame(label = c("Original assessment<br>format")), aes(label = label, x = date("1994-05-01"), y = 200), hjust=0, vjust=0, size = 6, family = "PublicoText-Roman", fill = NA, label.color = NA)
    if(i >= 105){
      g <- g +
        geom_richtext(data = data.frame(label = c("Revised assessment<br>format")), aes(label = label, x = date("2006-05-01"), y = 200), hjust=0, vjust=0, size = 6, family = "PublicoText-Roman", fill = NA, label.color = NA)
    }
    
    # add geom_point to g based on i
    X_axis_years <- unique(df$Year)
    Current_years <- unique(subset(tf, .frame <= i)$Year)
    a <- X_axis_years %in% Current_years
    
    # add the geom_point for the first year (for all frames)
    g <- g + geom_point(data = subset(tf, Year %in% X_axis_years[1]),aes(group=Category, color=Category), size=6)
    
    # add the geom_point for the last year (if i is the last frame)
    if(i == max(tf$.frame)) {
      g <- g + geom_point(data = subset(tf, Year %in% X_axis_years[length(X_axis_years)]),aes(group=Category, color=Category), size=6)
    }
    
    ### add y-axis breaks
    # define y-axis line objects and add them to the plot
    gline <- linesGrob(y = c(0, 1),x = c(-.015, .015),  gp = gpar(col = "#576F7F", lwd = 1)) 
    gline2 <- linesGrob(y = c(0, 1),x = c(0, 0),  gp = gpar(col = "white", lwd = 2))
    # y axis break - lower end
    g <- g +
      annotation_custom(gline2, ymin=186, ymax=189, xmin=-Inf, xmax=Inf) +
      annotation_custom(gline, ymin=187, ymax=191, xmin=-Inf, xmax=Inf) + 
      annotation_custom(gline, ymin=184, ymax=188, xmin=-Inf, xmax=Inf) +
      # y axis break - higher end
      annotation_custom(gline2, ymin=336, ymax=339, xmin=-Inf, xmax=Inf) +
      annotation_custom(gline, ymin=337, ymax=341, xmin=-Inf, xmax=Inf) + 
      annotation_custom(gline, ymin=334, ymax=338, xmin=-Inf, xmax=Inf) 
    
    # grobs are placed under the axis lines....
    plot <- ggplotGrob(g)
    plot$layout$clip[plot$layout$name=="panel"] <- "off"
    plot$layout$z[plot$layout$name=="panel"] = 18  # Note that z for panel is 1.  Change it to something bigger so that the line will overwrite the grid.
    
    
    # # add geom_text to g based on i
    # geom_text(data = subset(tf, Year %in% X_axis_years[a]), size = 9, 
    #           aes(label = round(Value, 0)),
    #           nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
    #           nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
    #           family="PublicoText-Roman")
    
    #grob
    # ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
    #plot <- ggplotGrob(g)
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
movie.name=here("Code", "COE", "Results", "CNJ-4_v4.gif"),interval = .02, ani.width = 1200, ani.height = 800)

#compressing
gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

gif_compress("/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/COE/Results/CNJ-4_v4.gif","/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/COE/Results/CNJ-4_v4_compressed.gif")



