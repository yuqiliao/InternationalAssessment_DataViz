### TIMSS Trend data viz
### This is to be used as a tweet for the NCES handle
### 8/13/18
### Yuqi Liao


### Set things up ------
## install gganimate and its dependents
# install.packages("installr")
# library(installr)
# install.ImageMagick()

devtools::load_all("U:/ESSIN Task 14/NAEP R Program/Yuqi/edsurvey")

library(ggplot2) # devtools::install_github("hadley/ggplot2")
library(scales)
library(tidyr)
library(Cairo)
library(extrafont)
library(dplyr)
library(lubridate)
library(tweenr)
# library(gganimate) # gh_install_packages("dgrtwo/gganimate", ref = "26ec501") #devtools::install_github("dgrtwo/gganimate")
library(animation)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(directlabels)

# Sys.setenv(PATH = paste("C:/PROGRA~1/ImageMagick-7.0.8-Q16",
#                         Sys.getenv("PATH"), sep = ";"))
# ani.options(convert = 'C:/PROGRA~1/ImageMagick-7.0.8-Q16/convert.exe')




### download/clean data  -----
T95_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T1995/TIMSS/Grade 08/Y1995/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T99_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T1999/TIMSS/Grade 08/Y1999/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T03_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T2003/TIMSS/Grade 08/Y2003/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T07_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T2007/TIMSS/Grade 08/Y2007/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T11_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T2011/TIMSS/Grade 08/Y2011/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")
T15_G8_USA <- readTIMSS(path = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Data/TIMSS/T2015/TIMSS/Grade 08/Y2015/Data/SPSS/",
                        countries = "usa",
                        gradeLvl = "8")


# for T15_G8_USA, recode the level in 2015 to make it consistent with previous cycles
T15_G8_USA <- recode.sdf(T15_G8_USA,
                         recode = list(itsex = list(from = "FEMALE",
                                                    to = "GIRL"),
                                       itsex = list(from = "MALE",
                                                    to = "BOY")))

# for T95_G8_USA, keep only the students who were grade 8
T95_G8_USA_filtered <- subset(T95_G8_USA,idgrade %in% 8)

# combine all data sets
TIMSSAllYears_G8_USA <- edsurvey.data.frame.list(list(T95_G8_USA_filtered, T99_G8_USA, T03_G8_USA, T07_G8_USA, T11_G8_USA, T15_G8_USA),
                                                 labels= c("T95_G8_USA", "T99_G8_USA", "T03_G8_USA", "T07_G8_USA", "T11_G8_USA", "T15_G8_USA"))

# gap anaysis
ssciGap_itsex <- gap(variable = 'ssci', data = TIMSSAllYears_G8_USA,
                     groupA = itsex %in% "GIRL", groupB = itsex %in% "BOY")


# contruct data frame for visualization
ssci_itsex <- ssciGap_itsex$results %>% 
  select(labels, estimateA, estimateB) %>% 
  mutate(year = c("1995", "1999", "2003", "2007", "2011", "2015")) %>% 
  rename(Girl = estimateA,
         Boy = estimateB) %>% 
  select(-labels) %>% 
  gather(key = "Gender",
         value = "ScienceScore",
         -year)

# save/load the datasets
save.image(file = "G:/Data Science/Data_Viz/Github/data-viz/International Assessment/RObjects.Rdata")
load("G:/Data Science/Data_Viz/Github/data-viz/International Assessment/RObjects.Rdata")


### Visualize -----

# clean the original data frame
df <- ssci_itsex
colnames(df)[1] <- "Year"
colnames(df)[2] <- "Category"
colnames(df)[3] <- "Value"

df$Category <- as.factor(df$Category)
df$Year <- as.Date(as.character(df$Year),"%Y")

# function to create list of data sets from the original data frame (by state)
states1<-unique((df$Year))
myf<-function(mystate){as.data.frame(df[df$Year==as.Date(mystate),])}

# use lapply to generate the list of data sets:
my.list1<-lapply(states1,myf)

# Apply tweenr: this adds in values between our data points to "smooth" the line
tf <- tween_states(my.list1, tweenlength= 1, statelength=0, ease='linear',nframes=80)
tf$Year <- as.Date(tf$Year,"%Y")

# needed to add in last 2 data points so that they match exactly the final year
temp <- tf[(max(tf$.frame) - 1):max(tf$.frame),]
temp$.frame <- max(tf$.frame) + 1
temp2 <- df %>% filter(Year == max(Year))
temp$Year <- NULL
temp$Category <- NULL
temp$Value <- NULL
temp <- cbind(temp2, temp)
tf <- rbind(tf, temp)

# # create factor variable to specify the labels we'd like to appear in the chart, then copy that column to a new variable as a character
# tf$Category <- factor(tf$Category, 
#                       levels = c("Master of business  administration (M.B.A.)", "Master of education (any)", "Master of arts (M.A.), except in education", "Master of science (M.S.), except in education"), 
#                       labels = toupper(c("Master of business\nadministration (M.B.A.)", "Master of education (any)", "Master of arts (M.A.),\nexcept in education", "Master of science (M.S.),\nexcept in education"))
# )
# tf$CategoryLabel <- as.character(tf$Category)


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

# NCES color palette
my_palette =  c("#fbab18", "#3FA66C", "#3EC7F4","#242953")

# NCES color palette, lightened for the grid backgrounds
my_palette_tint <- c("#fef1d9", "#def2e7", "#d2f2fc","#e5e6f4")

# function to draw the same figure n-many times, so that the animation "pauses"
gifReplicate <- function(x) {
  grid.newpage()
  grid.draw(x)
}


# y axis break labels and levels
yAxisBreaks <- seq(490, 560, by = 10) #the data set at hand ranges from 505 to 535, so i decide to make it from 490 to 560 to account for y axis breaks
yAxisLabels <-c("0", seq(500, 550, by = 10), "1,000") #yAxisLabels <- paste0(format(seq(500, 550, by = 10), big.mark = ","))
yAxisLimits <-  range(yAxisBreaks)      #yAxisLimits <- c(min(yAxisBreaks)/ 1.01,max(yAxisBreaks)* 1.01)
#yAxisLabels <- c(yAxisLabels[1:length(yAxisLabels)-1], paste0("Science Score\n",yAxisLabels[length(yAxisLabels)]))

# this section covers 
# x axis break labels and levels
xAxis <- unique(df$Year)
xAxisBreaks <- paste0(year(unique(df$Year)), "*")

# if there is a label that stays next to the line over the course of the visualization, how much should it be moved over?
tf$nudge_x <- 0
tf$nudge_y <- 0
# first and last data points, some of which need to be adjusted left, right, up, or down based on the position of the data point in relation to the grid
tf[tf$Year %in% xAxis & tf$Category=="Girl", ]$nudge_y <- -2
tf[tf$Year %in% xAxis & tf$Category=="Boy", ]$nudge_y <- 2

tf[tf$Year %in% xAxis & tf$Category=="Girl", ]$nudge_x <- -0
tf[tf$Year %in% xAxis & tf$Category=="Boy", ]$nudge_x <- 0

# plot labels - caption uses expression to add in italics and to stack lines on top of each other
title <- "TIMSS Science Performance of 8th-Grade Students in the US, by Gender: 1995\u20132015"
subtitle <- "The US gender gap in science is shriking over the years"
# caption <- expression("SOURCE: U.S. Department of Education, National Center for Education Statistics, "~italic("Digest of Education Statistics 2017")~', Table 332.45.')
caption <- expression(atop("NOTE: * The score gap is statistically significant in the selected cycle.                                                                                                                                                        ","SOURCE: International Association for the Evaluation of Educational Achievement (IEA), Trends in International Mathematics and Science Study (TIMSS), 1995\u20132015"))


# create GIF - version: pause at start/end ----- 
saveGIF({

firstFig <-  ggplot(tf, aes(x = Year, y = Value)) +
  geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=10) + 
  scale_x_date(labels=xAxisBreaks, expand = c(0.01, 0), breaks=xAxis,limits =as.Date(c("1994-08-13", "2016-08-13"))) +                           
  scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
  geom_text(data = subset(tf, .frame == min(.frame)), size = 9, 
            aes(label = round(Value, 0)),
            nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
            nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
            family="Calibri") + 
  theme_minimal() + theme_white + 
  scale_color_manual(values=my_palette) + scale_fill_manual(values=my_palette) +
  labs(x="Year", y="", title = title, #subtitle = subtitle, 
       caption = caption) +
  geom_dl(data = subset(tf, .frame == min(.frame)),
          aes(label = Category), method = list("last.points", cex = 2, hjust = -0.5))


# ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
 g <- ggplotGrob(firstFig)
# g$layout$l[g$layout$name == "title"] <- 3
# g$layout$l[g$layout$name == "caption"] <- 3
# g$layout$l[g$layout$name == "subtitle"] <- 3
grid::grid.draw(g);

# replicate the same figure n-many times, so that the animation "pauses" 
replicate(30,gifReplicate(g))
grid.newpage()

# from the 1st to the max frame of the full length of the data set, take the i-th value and generate a visualization
for (i in 1:max(tf$.frame)) {
  print(paste0("working on the ", i, "th frame"))
  g <- ggplot(data = subset(tf, .frame <= i), aes(x = Year, y = Value, .frame = i)) +
    geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=10) + 
    geom_line(aes(group=Category, color=Category), size=2.5) +
    scale_x_date(labels=xAxisBreaks, expand = c(0.01, 0), breaks=xAxis,limits =as.Date(c("1994-08-13", "2016-08-13"))) +                           
    scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
    geom_text(data = subset(tf, .frame == min(.frame)), size = 9, 
              aes(label = round(Value, 0)),
              nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
              nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
              family="Calibri") + 
    theme_minimal() + theme_white + 
    scale_color_manual(values=my_palette) + scale_fill_manual(values=my_palette) +
    labs(x="Year", y="", title = title, #subtitle = subtitle, 
         caption = caption) +
    geom_dl(data = subset(tf, .frame <= i),
            aes(label = Category), method = list("last.points", cex = 2, hjust = -0.5))

  # ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
  g <- ggplotGrob(g)
  # g$layout$l[g$layout$name == "title"] <- 3
  # g$layout$l[g$layout$name == "caption"] <- 3
  # g$layout$l[g$layout$name == "subtitle"] <- 3
  
  # draw the figure at the i-th value of the data set
  grid::grid.draw(g); 
  grid.newpage()
}

# the full figure
wholeFig <- ggplot(tf, aes(x = Year, y = Value)) +
  geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=10) + 
  geom_point(data = subset(tf, .frame == max(.frame)),aes(group=Category, color=Category), size=10) +
  geom_line(aes(group=Category, color=Category), size=2.5) +
  scale_x_date(labels=xAxisBreaks, expand = c(0.01, 0), breaks=xAxis,limits =as.Date(c("1994-08-13", "2016-08-13"))) +                           
  scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
  geom_text(data = subset(tf, .frame == min(.frame)), size = 9, 
            aes(label = round(Value, 0)),
            nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
            nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
            family="Calibri") + 
  geom_text(data = subset(tf, .frame == max(.frame)), size = 9, 
            aes(label = round(Value, 0)),
            nudge_y = subset(tf, .frame == max(.frame))$nudge_y, 
            nudge_x= subset(tf, .frame == max(.frame))$nudge_x, 
            family="Calibri") +
  theme_minimal() + theme_white + 
  scale_color_manual(values=my_palette) + scale_fill_manual(values=my_palette) +
  labs(x="Year", y="", title = title, #subtitle = subtitle, 
       caption = caption) +
  geom_dl(data = tf,
          aes(label = Category), method = list("last.points", cex = 2, hjust = -0.5))

# ggplotGrob is used to capture the figure in the graphics device, then shift the plot labels to the left most part of the plot
g <- ggplotGrob(wholeFig)
# g$layout$l[g$layout$name == "title"] <- 3
# g$layout$l[g$layout$name == "caption"] <- 3
# g$layout$l[g$layout$name == "subtitle"] <- 3
grid::grid.draw(g);

# replicate the same figure n-many times, so that the animation "pauses" 
replicate(250,gifReplicate(g))

},
# specify the pathway and name of the gif output, as well as the interval, width, and height
movie.name="G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Output/ssci_itsex.gif",interval = .02, ani.width = 1200, ani.height = 1000)



# create GIF - version: pause in each year ----- 

saveGIF({
for (i in 1:max(tf$.frame)) {
  print(paste0("working on the ", i, "th frame"))
  g <- ggplot(data = subset(tf, .frame <= i), aes(x = Year, y = Value, .frame = i)) +
    geom_line(aes(group=Category, color=Category), size=2.5) +
    scale_x_date(labels=xAxisBreaks, expand = c(0.01, 0), breaks=xAxis,limits =as.Date(c("1994-08-13", "2016-08-13"))) +                           
    scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
    theme_minimal() + theme_white + 
    scale_color_manual(values=my_palette) + scale_fill_manual(values=my_palette) +
    labs(x="Year", y="", title = title, #subtitle = subtitle, 
         caption = caption) +
    geom_dl(data = subset(tf, .frame <= i),
            aes(label = Category), method = list("last.points", cex = 2, hjust = -0.5))
  
  # add geom_point to g based on i
  X_axis_years <- unique(df$Year)
  Current_years <- unique(subset(tf, .frame <= i)$Year)
  a <- X_axis_years %in% Current_years
  
  g <- g + geom_point(data = subset(tf, Year %in% X_axis_years[a]),aes(group=Category, color=Category), size=10) +
  # add geom_text to g based on i
  geom_text(data = subset(tf, Year %in% X_axis_years[a]), size = 9, 
            aes(label = round(Value, 0)),
            nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
            nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
            family="Calibri")
  
  # define y-axis line objects and add them to the plot
  gline <- linesGrob(y = c(0, 1.5),x = c(-.015, .015),  gp = gpar(col = "black", lwd = 2)) 
  gline2 <- linesGrob(y = c(-0.25, 0.5),x = c(0, 0),  gp = gpar(col = "white", lwd = 2))
  
  g <- g + 
    # y axis break - lower end
    annotation_custom(gline, ymin=495, ymax=496, xmin=-Inf, xmax=Inf) + 
    annotation_custom(gline, ymin=494, ymax=495, xmin=-Inf, xmax=Inf) +
    annotation_custom(gline2, ymin=495, ymax=496, xmin=-Inf, xmax=Inf) +
    # y axis break - higher end
    annotation_custom(gline, ymin=555, ymax=556, xmin=-Inf, xmax=Inf) + 
    annotation_custom(gline, ymin=554, ymax=555, xmin=-Inf, xmax=Inf) +
    annotation_custom(gline2, ymin=555, ymax=556, xmin=-Inf, xmax=Inf) 
  
  # grobs are placed under the axis lines....
  plot = ggplotGrob(g)
  plot$layout$clip[plot$layout$name=="panel"] <- "off"
  plot$layout$z[plot$layout$name=="panel"] = 18  # Note that z for panel is 1.  Change it to something bigger so that the line will overwrite the grid.
  
  
  
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
      replicate(30,gifReplicate(plot))
    }

  } else {
    # just draw the plot one time
    grid::grid.draw(plot);
    grid.newpage()
  }

  
}  
},
# specify the pathway and name of the gif output, as well as the interval, width, and height
movie.name="G:/Data Science/Data_Viz/Github/data-viz/International Assessment/Output/ssci_itsex_v3.gif",interval = .02, ani.width = 1200, ani.height = 1000)






### this is the code to plot the final plot! -----


wholeFig <- ggplot(tf, aes(x = Year, y = Value)) +
  geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=10) + 
  geom_point(data = subset(tf, .frame == max(.frame)),aes(group=Category, color=Category), size=10) +
  geom_line(aes(group=Category, color=Category), size=2.5) +
  scale_x_date(labels=xAxisBreaks, expand = c(0.01, 0), breaks=xAxis,limits =as.Date(c("1994-08-13", "2016-08-13"))) +                           
  scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
  geom_text(data = subset(tf, .frame == min(.frame)), size = 9, 
            aes(label = round(Value, 0)),
            nudge_y = subset(tf, .frame == min(.frame))$nudge_y, 
            nudge_x= subset(tf, .frame == min(.frame))$nudge_x, 
            family="Calibri") + 
  geom_text(data = subset(tf, .frame == max(.frame)), size = 9, 
            aes(label = round(Value, 0)),
            nudge_y = subset(tf, .frame == max(.frame))$nudge_y, 
            nudge_x= subset(tf, .frame == max(.frame))$nudge_x, 
            family="Calibri") +
  theme_minimal() + theme_white + 
  scale_color_manual(values=my_palette) + scale_fill_manual(values=my_palette) +
  labs(x="Year", y="", title = title, #subtitle = subtitle, 
       caption = caption) +
  geom_dl(data = tf,
          aes(label = Category), method = list("last.points", cex = 2, hjust = -0.5))


gline = linesGrob(y = c(0, 1.5),x = c(-.015, .015),  gp = gpar(col = "black", lwd = 2)) 
gline2 = linesGrob(y = c(-0.25, 0.5),x = c(0, 0),  gp = gpar(col = "white", lwd = 2))

p = wholeFig + 
  # y axis break - lower end
  annotation_custom(gline, ymin=495, ymax=496, xmin=-Inf, xmax=Inf) + 
  annotation_custom(gline, ymin=494, ymax=495, xmin=-Inf, xmax=Inf) +
  annotation_custom(gline2, ymin=495, ymax=496, xmin=-Inf, xmax=Inf) +
  # y axis break - higher end
  annotation_custom(gline, ymin=555, ymax=556, xmin=-Inf, xmax=Inf) + 
  annotation_custom(gline, ymin=554, ymax=555, xmin=-Inf, xmax=Inf) +
  annotation_custom(gline2, ymin=555, ymax=556, xmin=-Inf, xmax=Inf) 

# grobs are placed under the axis lines....
p1 = ggplotGrob(p)
p1$layout$clip[p1$layout$name=="panel"] <- "off"
p1$layout$z[p1$layout$name=="panel"] = 18  # Note that z for panel is 1.  Change it to something bigger.
grid.newpage()
grid.draw(p1)









