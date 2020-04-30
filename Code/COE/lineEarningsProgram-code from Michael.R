library(ggplot2) # devtools::install_github("hadley/ggplot2")
library(scales)
library(tidyr)
library(Cairo)
library(extrafont)
library(dplyr)
library(lubridate)
library(tweenr)
library(gganimate) # gh_install_packages("dgrtwo/gganimate", ref = "26ec501")
library(animation)
library(RColorBrewer)
library(grid) 
library(gridExtra)
library(ggtext)
loadfonts()
# setwd("G:\\nces_theme\\working\\")
setwd("/home/michael/Documents/NCESgifs")

res <- read.csv("lineEarningsProgram/lineEarningsProgram.csv")

res$Category <- factor(res$Category,  labels = c("Business", "Computer\nSciences", 
"Education", "Engineering", "Health\nprofessions"), levels = c("Business", "Computer Sciences", 
"Education", "Engineering", "Health professions"))

res$Year <- as.Date(as.character(res$Year),"%Y")

#add the "US" at the top and the bottomw of the list of states
states1<-unique((res$Year))


# function to create list of data sets from our data (by state)
myf<-function(mystate){as.data.frame(res[res$Year==as.Date(mystate),])}

# use lapply to generate the list of data sets:
my.list1<-lapply(states1,myf)

# Apply tweenr:
# originally had 400 frames - split into 315 and 85, then reduced the nframes for the first list to 60 so it moves faster
tf1a <- tween_states(my.list1[1:2], tweenlength= 1, statelength=0, ease='linear',nframes=65)
tf1b <- tween_states(my.list1[2:4], tweenlength= 1, statelength=0, ease='linear',nframes=52)
tf1c <- tween_states(my.list1[4:5], tweenlength= 1, statelength=0, ease='linear',nframes=13)
tf2 <- tween_states(my.list1[5:length(my.list1)], tweenlength= 1, statelength=0, ease='linear',nframes=104)
tf1b$.frame <- max(tf1a$.frame) + tf1b$.frame
tf1c$.frame <- max(tf1b$.frame) + tf1c$.frame
tf2$.frame <- max(tf1c$.frame) + tf2$.frame
tf <- rbind(tf1a, tf1b, tf1c, tf2)
tf$Year <- as.Date(tf$Year,"%Y")



tf$CategoryLabel <- as.character(tf$Category)


theme_white <- theme(text = element_text(family="Publico Text", color = "black"),
                     # axis.title.y = element_blank(),
                     # axis.title.x = element_text(size=14),
                     # # panel.background=element_blank(),
                     # panel.border=element_rect(color="transparent"),
                     # # plot.margin = unit(c(.5, .5, .5, .5), "cm"),
                     panel.grid = element_blank(), panel.border = element_blank(),
                     # # axis.line.x=element_line(color="black"),
                     # # axis.line.y=element_line(color="black"),
                     axis.title.x=element_text(size=22, margin = margin(t=15, b = 5), hjust = .46, face = "bold"),
                     axis.text.x=element_text(size=18, angle = 0, hjust = .5, family = "Publico Text"),
                     axis.text.y=element_text(size=18, family = "Publico Text"),
                     # axis.line.x=element_line(color="#808184", size=.5),
                     # axis.line.y=element_line(color="#808184", size=.5),
                     axis.ticks = element_blank(),
                     # # plot.margin=unit(rep(0.5, 4), "cm"),
                     # # axis.text.y=element_text(margin=margin(r=-5)),                     
                     plot.title=element_text(size=30,family = "Publico Text",hjust= 0,lineheight=1, margin = margin(t = 15), face = "bold"),
                     plot.subtitle=element_text(size=22, margin = margin(t=15, b = 5),family = "Publico Text", face = "bold"),
                     plot.caption=element_markdown(size=17, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "Publico Text"),
                     legend.position="none"
)

my_palette =  c("#FBB03B", "#489FDF","#00843D", "#071D49", "#9E6A38")


gifReplicate <- function(x) {
  grid.newpage()
  grid.draw(x)
}

yAxisBreaks <- seq(0, 200000, by = 20000)
yAxisLabels <- paste0(formatC(yAxisBreaks, format="f", big.mark=",", digits=0))
yAxisLimits <- c(-1,max(yAxisBreaks)* 1.03)

# yAxisLabels <- c(yAxisLabels[1:length(yAxisLabels)-1], paste0(yAxisLabels[length(yAxisLabels)],"%"))

tf$nudge_y <- ifelse(tf$Category=="Engineering", 3500, ifelse(tf$Category %in% c("Computer\nSciences"), -6000, 0))
# tf$nudge_x <- ifelse(tf$Category=="Business", 450, ifelse(tf$Category=="Education", 525, ifelse(tf$Category=="Engineering", 600, ifelse(tf$Category %in% c("Health professions","Computer Sciences"), 800, 0))))


# tf$nudge_x <- 0
# tf$nudge_y <- 0

# xAxis <- unique(res$Year)
xAxisOrig <- c(2001, seq(2006,2011, by = 5), 2018)
xAxis <- paste0(xAxisOrig-1, "\u2013",substr(xAxisOrig, 3,4))

xAxisBreaks <- xAxisOrig
xAxisBreaks <- as.Date(as.character(xAxisBreaks),"%Y") -7


title <- "Number of master's degrees conferred by postsecondary institutions in\nselected fields of study: Academic years 2000\u201301 through 2017\u201318"
subtitle <- "Number of degrees"



caption <- "<span>SOURCE: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data<br>
   System (IPEDS), Fall 2001 through Fall 2018, Completions component. See <i>Digest of Education Statistics 2012</i>, table 314; <br>
   <i>Digest of Education Statistics 2018,</i> table 323.10.</span>"
              
# caption <- expression(
#                       atop(textstyle('U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS),'),
#                       atop(textstyle("Fall 2001 through Fall 2017, Completions component. See"~italic("Digest of Education Statistics 2012,")~"table 314;"~italic("Digest of Education Statistics 2018,")~"table 323.10"))
#                       ))

saveGIF({
  

  for (i in 1:max(tf$.frame)) {
    g <-  ggplot(data = subset(tf, .frame <= i), aes(x = Year, y = Value, .frame = i)) +
      geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=5) + 
      # geom_text(data = subset(tf, .frame == min(.frame)),aes(label = paste0(format(subset(tf, .frame == min(.frame))$Value, big.mark = ","))), size = 9, nudge_y= tf$nudge,nudge_x= -200,family="Publico Text") +
      geom_line(aes(group=Category, color=Category, cumulative = TRUE, label=CategoryLabel), size=2.5) +
      scale_x_date(labels=xAxis, expand = c(0, 0), breaks=xAxisBreaks, limits =as.Date(c("2000-07-01", "2022-01-01"))) +                     
      scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) + theme_minimal() + theme_white + scale_color_manual(values=my_palette) +
      geom_text(data = subset(tf, .frame == i),aes(label =CategoryLabel, hjust = 0, x = Year + 100), size = 8, nudge_y = subset(tf, .frame == i)$nudge_y, family="Publico Text", lineheight = 0.75) 

      # geom_text(data = subset(tf, .frame == 1), aes(label =  paste0(29, "%")), size = 9, family="Publico Text", nudge_y = -2.5)
    g <- g +  labs(x="Year", y="", title = title, subtitle = subtitle, 
                 caption = caption)
    g <- ggplotGrob(g)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    grid::grid.draw(g); 
    grid.newpage()
  }
  
  wholeFig <-  ggplot(tf, aes(x = Year, y = Value)) +
    geom_point(data = subset(tf, .frame == min(.frame)),aes(group=Category, color=Category), size=5) + 
    geom_point(data = subset(tf, .frame == max(.frame)),aes(group=Category, color=Category), size=5) + 
    # geom_text(data = subset(tf, .frame == min(.frame)),aes(label = paste0(" ", sprintf("%0.0f", subset(tf, .frame == min(.frame))$Value), "%")), size = 9,nudge_y= tf$nudge,nudge_x= -200,family="Publico Text") +
    geom_line(aes(group=Category, color=Category, cumulative = TRUE, label=CategoryLabel), size=2.5) +
    scale_x_date(labels=xAxis, expand = c(0, 0), breaks=xAxisBreaks, limits =as.Date(c("2000-07-01", "2022-01-01"))) +                 
    scale_y_continuous(labels=yAxisLabels, expand = c(0, 0), breaks=yAxisBreaks,limits = yAxisLimits) +
    # geom_text(data = subset(tf, .frame == 1), aes(label =  paste0(29, "%")), size = 9, family="Publico Text", nudge_y = -2.5)  +
    theme_minimal() + theme_white + scale_color_manual(values=my_palette) + 
    # geom_text(data = subset(tf, .frame == max(.frame)),aes(label = paste0(20,"%")), size = 9, nudge_y = -2, family="Publico Text")
    geom_text(data = subset(tf, .frame == max(.frame)),aes(label =paste0(CategoryLabel), hjust = 0, x = Year + 100), size = 8, nudge_y = subset(tf, .frame == max(.frame))$nudge_y, family="Publico Text", lineheight = 0.75)  + 
    labs(x="Year", y="", title = title, subtitle = subtitle, 
                 caption = caption)
  wholeFig
    g <- ggplotGrob(wholeFig)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    grid::grid.draw(g);

    for(i in 1:149) {
      grid.newpage()
      grid::grid.draw(g)
    }
    
# },movie.name="/home/michael/Documents/NCESgifs/lineEarningsProgram/lineEarningsProgramLarge1.gif",interval = .02, ani.width = 1112, ani.height = 632)
},movie.name="/home/michael/Documents/NCESgifs/lineEarningsProgram/lineEarningsProgramLarge.gif",interval = .02, ani.width = 1200, ani.height = 800)

gc()
gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

# gif_compress("/home/michael/Documents/NCESgifs/lineEarningsProgram/lineEarningsProgramLarge1.gif","/home/michael/Documents/NCESgifs/lineEarningsProgram/lineEarningsProgram.gif")
gif_compress("/home/michael/Documents/NCESgifs/lineEarningsProgram/lineEarningsProgramLarge.gif","/home/michael/Documents/NCESgifs/lineEarningsProgram/lineEarningsProgramTall.gif")