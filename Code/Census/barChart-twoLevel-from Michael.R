# install.packages(c("animation", "tweenr", "dplyr"))
# install.packages("RColorBrewer")
# install.packages("extrafont")

library(ggplot2)
library(tweenr)
library(dplyr)
library(RColorBrewer)
library(animation)
library(grid)
library(gridExtra)
library(stringr)
library(extrafont)
library(ggtext)

# setwd("G:\\nces_theme\\working\\")
setwd("/home/michael/Documents/NCESgifs")


##################################################################
## NOTE: only necessary if running the first time per R version!
# font_import() # takes a few minutes
# y
# font_import(paths = "/home/michael/Documents/NCESgifs/publico_text")
# y
##################################################################
extrafont::loadfonts()

res <- read.csv("barChartEducationalAttainment/barChartEducationalAttainment.csv", stringsAsFactors = FALSE)
# 
# res <- res %>% gather(Category, key= "Level")
# colnames(res)[3] <- "Ethnicity"
# colnames(res)[4] <- "Value"
# res <- subset(res, Level == "Bachelors" & Ethnicity != "Total")
# res$Level <- as.factor(unique(res$Level))


# res$Category <- ifelse(res$Category == "High school completion only", "High school completion only", res$Category)

res$Category <- factor(res$Category,
  levels = c("Less than high school completion", "High school completion only", "Some college or associate's degree", 
"Bachelor's or higher degree"),
  labels = c("Less than high school completion", "High school completion only", "Some college or associate's degree", 
"Bachelor's or higher degree"))

# res <- res %>% filter(Category %in% c("Less than high school completion", "High school completion only", "Some college or associate's degree", 
# "Bachelor's or higher degree", "Master's or higher degree"))


res <- res %>% filter(Category %in% c("Less than high school completion", "High school completion only", "Some college or associate's degree", 
"Bachelor's or higher degree"))

# res$Category <- as.Date(as.character(res$Category),"%Y")
# # res$Category <- as.POSIXct(res$Category, origin="1970-01-01")

res$Facet <- factor(res$Facet,
  levels = c("Internet always available", "Computer always available"),
  labels = c("Internet always available", "Computer always available"))

facet1 <- "Internet always available"
facet2 <- "Computer always available"

resFacet1 <- res %>% filter(Facet == facet1)
resFacet2 <- res %>% filter(Facet == facet2)
resFacet1color <- resFacet1[seq(dim(resFacet1)[1],1),]
resFacet1color$color <- rep(c("blue","white"))

#add the "US" at the top and the bottomw of the list of states
states1<-unique((resFacet1$Value))
myf<-function(mystate){as.data.frame(resFacet1[resFacet1$Value==mystate,])}

# use lapply to generate the list of data sets:
my.list1<-lapply(states1,myf)

# Apply tweenr:
# originally had 400 frames - split into 315 and 85, then reduced the nframes for the first list to 60 so it moves faster
tf <- tween_states(my.list1, tweenlength= 2, statelength=0, ease='cubic-in-out',nframes=50)


#add the "US" at the top and the bottomw of the list of states
states1<-unique((resFacet2$Value))
myf<-function(mystate){as.data.frame(resFacet2[resFacet2$Value==mystate,])}

# use lapply to generate the list of data sets:
my.list2<-lapply(states1,myf)

# Apply tweenr:
# originally had 400 frames - split into 315 and 85, then reduced the nframes for the first list to 60 so it moves faster
tfFacet1 <- tween_states(my.list1, tweenlength= 2, statelength=0, ease='cubic-in-out',nframes=50)
tfFacet2 <- tween_states(my.list2, tweenlength= 2, statelength=0, ease='cubic-in-out',nframes=50)



#add the "US" at the top and the bottomw of the list of states
states1<-unique((resFacet1color$Value))
myf<-function(mystate){as.data.frame(resFacet1color[resFacet1color$Value==mystate,])}

# use lapply to generate the list of data sets:
my.listcolor<-lapply(states1,myf)

# Apply tweenr:
# originally had 400 frames - split into 315 and 85, then reduced the nframes for the first list to 60 so it moves faster
tfFacetColor <- tween_states(my.listcolor, tweenlength= 2, statelength=0, ease='cubic-in-out',nframes=50)


tfFacet1$Category <- as.factor(tfFacet1$Category)

# theme_white <- theme(text = element_text(family="Arial Semibold", color = "black"),
#                      # axis.title.y = element_blank(),
#                      # axis.title.x = element_text(size=14),
#                      # # panel.background=element_blank(),
#                      # panel.border=element_rect(color="transparent"),
#                      # # plot.margin = unit(c(.5, .5, .5, .5), "cm"),
#                      panel.grid = element_blank(), panel.border = element_blank(),
#                      # # axis.line.x=element_line(color="black"),
#                      # # axis.line.y=element_line(color="black"),
#                      axis.title.x=element_text(size=26, margin = margin(t=15, b = 5)),
#                      axis.text.x=element_text(size=20, family = "Arial", margin= margin(t = -10)),
#                      axis.text.y=element_text(size=18, family = "Arial"),
#                      # axis.line.x=element_line(color="#808184", size=.5),
#                      # axis.line.y=element_line(color="#808184", size=.5),
#                      axis.ticks = element_blank(),
#                      # # plot.margin=unit(rep(0.5, 4), "cm"),
#                      # # axis.text.y=element_text(margin=margin(r=-5)),                    
#                      plot.title=element_text(size=24,family = "Arial",hjust= 0,lineheight=1.15, face = "bold"),
#                      plot.subtitle=element_text(size=23, margin = margin(t=20, b = 5),family = "Arial Semibold"),
#                      plot.caption=element_text(size=16, hjust = 0,margin=margin(t=15),family = "Arial"),
#                      # legend.position=c(.055,.96),
#                      legend.position="bottom",
#                      legend.title = element_text(size = 18, family = "Arial Semibold"),
#                      legend.text = element_text(size = 22, family = "Arial")
# ) 


theme_white <- theme(text = element_text(family="Publico Text", color = "black"),
                     # axis.title.y = element_blank(),
                     # axis.title.x = element_text(size=14),
                     # panel.border=element_rect(color="transparent"),
                     # # plot.margin = unit(c(.5, .5, .5, .5), "cm"),
                     panel.grid = element_blank(), panel.border = element_blank(),
                     # axis.ticks = element_line( size=.1, color="#576F7F" ),
                     # # axis.line.x=element_line(color="black"),
                     # # axis.line.y=element_line(color="black"),
                     axis.title.x=element_text(size=22, margin = margin(t=15, b = 5), face = "bold"),
                     axis.text.x=element_text(size=18, angle = 0, hjust = .5, family = "Publico Text", margin= margin(t = -10)),
                     axis.text.y=element_text(size=18, family = "Publico Text"),
                     # axis.line.x=element_line(color="#808184", size=.5),
                     # axis.line.y=element_line(color="#808184", size=.5),
                     axis.ticks = element_blank(),
                     # # plot.margin=unit(rep(0.5, 4), "cm"),
                     # # axis.text.y=element_text(margin=margin(r=-5)),                     
                     plot.title=element_text(size=26,family = "Publico Text",hjust= 0,lineheight=.9, margin = margin(t = 15), face = "bold"),
                     plot.subtitle=element_text(size=22, margin = margin(t=15, b = 5),family = "Publico Text", face = "bold"),
                     plot.caption=element_markdown(size=17, hjust = 0,margin=margin(t=15, b = 15),lineheight=1.15, family = "Publico Text"),
                     legend.position="bottom",
                     legend.title = element_text(size = 22, family = "Publico Text", face = "bold"),
                     legend.text = element_text(size = 20, family = "Publico Text")
                     )

# + 
#                      theme(plot.background=element_rect(fill="#e0eaef", color = "transparent"),
#                            panel.border=element_blank(),
#                            panel.background=element_rect(fill="#e0eaef", color = "transparent"),
#                            legend.background=element_rect(fill="#e0eaef", color = "transparent"))

my_palette =  c("#3EC7F4","#242953", "#3FA66C")
my_palette =  c("#242953", "#3FA66C")

x <- seq(0,100, by = 10)
labels <- c(x[1:length(x)-1], paste0(x[length(x)],"%"))

gifReplicate <- function(x) {
  grid.newpage()
  grid.draw(x)
}


captionTop <- paste0()

title <- "Percentage of adults who reported that a computer and the Internet\nwere always available for educational purposes, by educational\nattainment of adult: April 23 through May 5, 2020"
# title <- "Internet always available full-time year-round workers ages 25\u201334 earn more than their Computer always available counterparts\nacross all educational attainment categories in 2018"
subtitle <- "Educational attainment of adult"

caption <- '<span>SOURCE: U.S. Department of Commerce, Census Bureau, Household Pulse Survey</span>'




                      

saveGIF({
  for (i in 1:max(tfFacet1$.frame)) {
    gg <- ggplot(subset(tfFacet1, .frame == i), aes(x = Category, y = Expenditure)) +
      geom_bar(aes(fill = Facet), stat = "identity") +
      scale_fill_manual(drop = FALSE, values = my_palette) +
      scale_y_continuous(labels = labels, breaks=seq(0, 100, by = 10),limits = c(0, 100)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

    gg <- gg + labs(x=subtitle, y="", title = title, fill = "",
                    subtitle = "Percent",
                    caption = caption)

    gg <- gg + theme_minimal() + theme_white 
    g <- ggplotGrob(gg)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    grid::grid.draw(g); 
    grid.newpage()
    print(paste0(i, " first bar"))
    rm(g)
  }
  tfFacet1$Expenditure <- round(tfFacet1$Expenditure, 0)
  gg <- gg + geom_text(data = subset(tfFacet1, .frame == max(tf$.frame)), aes(label = paste0(Expenditure, "%")), vjust = -1, color = "black", size = 8, family = "Publico Text") 
  
  g <- ggplotGrob(gg)
  g$layout$l[g$layout$name == "title"] <- 4
  g$layout$l[g$layout$name == "caption"] <- 4
  g$layout$l[g$layout$name == "subtitle"] <- 4
  grid.draw(g)
  replicate(48,gifReplicate(g))
  grid.draw(g)
  grid.newpage()
  print(paste0( "replicating..."))
  rm(g)

  vals <- seq(.05,1,by = .05)
  for (i in unique(vals)) {
    gg <- gg + geom_segment(data = tfFacet1[tfFacet1$.frame == max(tfFacet1$.frame),],  aes(x = Category, xend = Category, y = 0, yend = Expenditure - (Expenditure - (Expenditure * i)) - 1), size = 100, color = "white")

    gg <- gg + labs(x=subtitle, y="", title = title,
                    subtitle = "Percent",
                    caption = caption)

    g <- ggplotGrob(gg)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    grid::grid.draw(g); 
    grid.newpage()
    print(paste0(i, " unfurl"))
    if(i == vals[20]) {
      grid::grid.draw(g); 
      # grid.newpage()
    }
    rm(g)
  }
  for (i in 1:max(tfFacet2$.frame)) {
    gg <- gg + geom_segment(data = tfFacet1[tfFacet1$.frame == max(tfFacet1$.frame),],  aes(x = Category, xend = Category, y = 0, yend = Expenditure - (Expenditure - (Expenditure * 1)) - 1), size = 100, color = "white") +
      geom_bar(data = subset(tfFacet2, .frame == i), aes(fill = Facet), stat = "identity")

    gg <- gg + labs(x=subtitle, y="", title = title,
                    subtitle = "Percent",
                    caption = caption)

    g <- ggplotGrob(gg)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    grid::grid.draw(g); 
    grid.newpage()
    print(paste0(i, "  second bar"))
    rm(g)
  }

  tfFacet2$Expenditure <- round(tfFacet2$Expenditure, 0)
  gg <- gg  + geom_text(data = subset(tfFacet2, .frame == max(tf$.frame)), aes(label = paste0(Expenditure, "%")), vjust = 1.4, color = "white", size = 8, family = "Publico Text") 

    g <- ggplotGrob(gg)
    g$layout$l[g$layout$name == "title"] <- 4
    g$layout$l[g$layout$name == "caption"] <- 4
    g$layout$l[g$layout$name == "subtitle"] <- 4
    grid::grid.draw(g);

    print(paste0( "replicating final..."))

    replicate(300,gifReplicate(g))
    grid.draw(g)
},movie.name="/home/michael/Documents/NCESgifs/barChartEducationalAttainment/barChartEducationalAttainment.gif",interval = .02, ani.width = 900, ani.height = 900)

gc()

gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

gif_compress("/home/michael/Documents/NCESgifs/barChartEducationalAttainment/barChartEducationalAttainment.gif","/home/michael/Documents/NCESgifs/barChartEducationalAttainment/barChartEducational_Attainment.gif")