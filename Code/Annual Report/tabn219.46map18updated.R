### Annual report indicators data viz
### This is to be used as a gif tweet (animated map) for the NCES handle (based on Michael's code)
### 12/09/19 was given updated 2018 data, thus need to update the gif accordingly
### Yuqi Liao

### Set things up ------

# define and load all packages
reqpkg <- c("rgdal","rgeos", "gtable", "gganimate", "ggplot2", "grid", "maptools", "Cairo", "mapproj", "scales", "tweenr", "dplyr", "tidyr", "lubridate", "animation", "egg", "gtable", "here", "readxl")

sapply(reqpkg, function(pkgi) {
  if (!pkgi %in% installed.packages()) {
    install.packages(pkgi, repos = "http://cran.us.r-project.org")
  }
  library(pkgi, character.only = TRUE)
})

# inspect the working directory
here()
setwd(here("Code", "Annual Report", "Materials"))
#setwd("/home/michael/Documents/NCESgifs/mapGraduationRate")

dat <- read_excel(path = here("Code", "Annual Report", "Materials", "tabn219.46map18updated_clean.xlsx"),
                 sheet = "processed") 
dat <- dat %>% gather(Year, Percent, -State) %>% group_by(State)
colnames(dat)[2:3] <- c("Year","Percent")
dat$Year <- as.numeric(gsub("X", "", dat$Year))
dat$breaks <- cut(as.numeric(dat$Percent),
                      breaks=c(-1.1, 1, 69.99, 79.99, 89.99, 100),
                      labels=c(" Not available/Reporting\n standards not met", " Less than 70 percent", " 70 percent to less\n than 80 percent",
                               " 80 percent to less\n than 90 percent", " 90 percent or higher"))

legendLabels <- c(" Not available/Reporting\n standards not met", " Less than 70 percent", " 70 percent to less\n than 80 percent",
                               " 80 percent to less\n than 90 percent", " 90 percent or higher")
legendValues <- c(1:5)

square_grid <- function() {
  us <- readOGR("npr_stategrid.geojson", ,layer = "npr_stategrid")
  
  centers <<- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$abbr))
  us_map <<- fortify(us, region="abbr")
}

datAbb <- dat
stateName <- c(state.name, "District of Columbia")
stateAbb <- c(state.abb, "DC")
stateDF <- data.frame(cbind(stateAbb, stateName),stringsAsFactors =FALSE)

datAbb$State <- trimws(datAbb$State)
datAbb <- left_join(datAbb, stateDF, by = c("State" = "stateName"))

square_grid()

## remove DD and PR
us_map <- us_map %>% filter(id != "PR" & id != "DD")
centers <- centers %>% filter(id != "PR" & id != "DD")

datAbb$Year <- as.Date(ISOdate(datAbb$Year, 12, 31))


datAbb$breaksVal <-  ifelse(datAbb$breaks == " Not available/Reporting\n standards not met",1,
                                       ifelse(datAbb$breaks == " Less than 70 percent", 2,
                                              ifelse(datAbb$breaks == " 70 percent to less\n than 80 percent",3, 
                                                ifelse(datAbb$breaks == " 80 percent to less\n than 90 percent",4,5))))

datTF <- datAbb[,c(2,ncol(datAbb))]

myf<-function(mystate){as.data.frame(datTF[datTF$Year==as.Date(mystate),])}
states2<-factor(unique((datTF$Year)))
my.list2<-lapply(states2,myf)

tf <- tween_states(my.list2, tweenlength= 2, statelength=3, ease='exponential-in-out',nframes=200)
tf$State <- datAbb$stateAbb[1:51]
tf$Year <- as.Date(tf$Year,"%Y")


years <- unique(dat$Year)
yearData <- data.frame(line = c(rep(1,length(unique(datAbb$Year)))), Year = years, upper = c(rep(1.05,length(unique(datAbb$Year)))), lower = c(rep(0.95,length(unique(datAbb$Year)))),stringsAsFactors = FALSE)

# function to create list of data sets from our data (by state)
yearData$Year <- as.Date(ISOdate(yearData$Year, 12, 31))
myf<-function(mystate){as.data.frame(yearData[yearData$Year==as.Date(mystate),])}
states2<-factor(unique((yearData$Year)))
my.list2<-lapply(states2,myf)

tf2 <- tween_states(my.list2, tweenlength= 2, statelength=3, ease='exponential-in-out',nframes=200)
tf2$Year <- as.Date(tf2$Year,"%Y")


yearz <- year(as.Date(yearData$Year,"%d/%m/%Y")) - 1


temp <- subset(tf, .frame == 70)[51,]
temp2 <- temp
temp$breaksVal <- 1
temp$State <- "FA"
temp2$breaksVal <- 5
temp2$State <- "DA"

# ```
gifReplicate <- function(x) {
    grid.newpage()
    grid.draw(rectGrob(gp=gpar(fill="#e0eaef", lwd = 0)))
    fg <- gtable_rbind(timeLineFrame,mapFrame, size = "last")

    grid.draw(fg)  
}

saveGIF({
  for (i in 1:max(tf$.frame)) {
    ggMap <- ggplot()
    ggMap <- ggMap + geom_map(data=us_map, map=us_map,
                              aes(x=long, y=lat, map_id=id),
                              color="white", size=.5,show.legend = FALSE) +
      geom_map(data = rbind(subset(tf, .frame == i),temp,temp2), map=us_map, aes(fill=breaksVal, map_id=State, frame = .frame), color = "#242953") +
      geom_map(data = tf, map=us_map, aes(map_id=State, frame = .frame), alpha=0, color="#242953") +
      geom_text(data=centers, aes(label=id, x=x, y=y), show.legend = FALSE,color="#242953", size=6.5, family = "Gotham-Book") +
      scale_fill_gradient(high = "#3ec7f4", low= "#ffffff",
                          labels = c(" Not available/Reporting\n standards not met", " Less than 70 percent", " 70 percent to less\n than 80 percent",
                               " 80 percent to less\n than 90 percent", " 90 percent or higher"),
                          breaks = c(1:5))
    ggMap <- ggMap + 
      labs(x=NULL, y=NULL,title = NULL,caption = 'SOURCE: U.S. Department of Education, Office of Elementary and Secondary Education, Consolidated State\nPerformance Report, 2010\u201311 through 2017\u201318') +
      coord_map() + theme_bw()  + theme(plot.title=element_blank()) +
      theme(plot.caption=element_text(size=15, hjust=0, margin=margin(t= 15),lineheight=1.05, family = "Gotham-Book", color = "#242953")) +
      theme(panel.border=element_blank()) +
      theme(panel.grid=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(plot.background=element_rect(fill="#e0eaef")) + 
      theme(panel.background=element_rect(fill="#e0eaef")) + 
      theme(legend.background=element_rect(fill="#e0eaef")) + 
      theme(legend.text = element_text(size=15.5, color = "#242953", family = "Gotham-Book", lineheight = )) +
      theme(legend.title = element_text(size=18,family = "Gotham-Bold", color = "#242953")) +
      theme(legend.key.size = unit(2, 'lines')) +
      theme(legend.position=c(0.91, 0.2)) + theme(legend.direction="vertical") +
      theme(legend.key = element_rect(size = 5)) +
      guides(fill=guide_legend(values = legendValues, labels = legendLabels, title="Adjusted cohort\ngraduation rate", reverse=TRUE, keywidth = 2.5, keyheight = 2.5, title.vjust = 2))
    
    shift <- 1
    ggMap <- ggMap + ylim(36.75,45.5) + xlim(-106.25 - shift,-88.25 + shift)

    ggDots <- ggplot(tf2, aes(Year,line)) + geom_point(data = subset(tf2, .frame == i), aes(frame = Year),color = "#3ec7f4", size = 8) +
      geom_path(data = yearData, aes(Year), color = "#242953") + geom_linerange(data = yearData,aes(ymin = lower, ymax = upper), color = "#242953") +
      labs(x=NULL, y=NULL, title = "Adjusted cohort graduation rate (ACGR) of public high school\nstudents, by state: 2010\u201311 through 2017\u201318") +
      geom_text(data = yearData,aes(label = paste0(yearz,"\u2013", substr(yearz + 1, 3,4))), color = "#242953", size = 5.2, vjust = -1.5,family = "Gotham-Bold") +
      scale_y_continuous(limits = 1 + c(-.05, .2)) +
      theme(plot.title = element_text(size=28, vjust= .5, margin=margin(b= 20), color = "#242953", family = "Gotham-Bold")) +
      theme(panel.background=element_rect(fill="#e0eaef")) + theme(plot.background=element_rect(fill="#e0eaef")) + theme(panel.grid=element_blank()) +theme(axis.ticks=element_blank()) +
      theme(axis.text=element_blank()) 
    
    map <- ggplotGrob(ggMap)
    timeLine <- ggplotGrob(ggDots)
    
    
    mapFrame <- gtable_frame(map, width = unit(34,"cm"), height = unit(10, "cm"), debug = FALSE)
    timeLineFrame <- gtable_frame(timeLine, width = unit(34,"cm"), height = unit(1.8, "cm"), debug = FALSE)
    
    if (i != 1) {
      grid.newpage()
    }
    grid.draw(rectGrob(gp=gpar(fill="#e0eaef", lwd = 0)))
    fg <- gtable_rbind(timeLineFrame,mapFrame, size = "last")

    grid.draw(fg)

    }


    replicate(100,gifReplicate(fg))
    grid.draw(fg)

},movie.name="tabn219.46map18data.gif",interval = .05, title_frame = FALSE, ani.width = 1050, ani.height = 800)

gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

gif_compress("/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/Annual\\ Report/Results/tabn219.46map18data.gif","/Users/Yuqi/Desktop/Files/AIR/GIT/InternationalAssessment_DataViz/Code/Annual\\ Report/Results/tabn219.46map18data_compressed.gif")

