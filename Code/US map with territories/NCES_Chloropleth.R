##################################################################
# Set working directory: wherever the data file exists
wd <- "/Users/ahearn/Desktop/AIR/Projects/automatedFigures/Map/Deliverable/"
wd <- "C:/GIT/InternationalAssessment_DataViz/Code/US map with territories/"
setwd(wd)

# Name of Excel or CSV file that will feed the map
data = 'mapData.csv' 

# Output file type: can be eps, svg, png, jpg, pdf. 
output = 'png' 

# Reverse scale
rev_scale = FALSE
##################################################################



# Install and load packages
{
# Install package libraries
reqpkg <- c("sf","sp","rgeos","maptools","ggplot2","viridis",
  "scales","purrr","readxl","showtext","dplyr","extrafont","grDevices", 
  "tigris", 'stringr')

sapply(reqpkg, function(pkgi) {
  rr <- require(package=pkgi, character.only=TRUE)
  if(!rr) {
    install.packages(pkgi)
  }
})

# Albers projection
if(!require(package="albersusa", character.only=TRUE)) {
  devtools::install_github("hrbrmstr/albersusa")
}

# load libraries
library(albersusa) # devtools::install_github("hrbrmstr/albersusa")
library(sf)
library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggpattern)
library(viridis)
library(scales)
library(purrr)
library(readxl)
library(showtext)
library(dplyr)
library(extrafont)
library(grDevices)
library(tigris)
library(stringr)
us_states = states()
}

##################################################################
## NOTE: only necessary if running the first time per R version!
## (Museo Slab is NCES official font)

#font_import(paths = '/System/Library/Fonts/')
#y
font_import(paths = "C:/GIT/InternationalAssessment_DataViz/Code/US map with territories/museo-slab")
loadfonts(device="postscript")

##################################################################



# calculate the centroids (geographic center) of usa albers projection for state labels
# retrieve the x/y coordinates of the centroids which we can manipulate to customize their positioning
usaMap <-
  st_as_sf(usa_composite("laea")) %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

#  customize location of labels for small/odd state shapes
usaMap[usaMap$iso_3166_2=="MI",]$COORDS_X <- 1245000
usaMap[usaMap$iso_3166_2=="MI",]$COORDS_Y <- -100000

usaMap[usaMap$iso_3166_2=="LA",]$COORDS_X <- 700000
usaMap[usaMap$iso_3166_2=="LA",]$COORDS_Y <- -1450000

usaMap[usaMap$iso_3166_2=="FL",]$COORDS_X <- 1825000
usaMap[usaMap$iso_3166_2=="FL",]$COORDS_Y <- -1700000

#  customize location of labels for small/odd state shapes
usaMap[usaMap$iso_3166_2=="RI",]$COORDS_X <- 2600000
usaMap[usaMap$iso_3166_2=="RI",]$COORDS_Y <- -30000

usaMap[usaMap$iso_3166_2=="CT",]$COORDS_X <- 2500000
usaMap[usaMap$iso_3166_2=="CT",]$COORDS_Y <- -150000

usaMap[usaMap$iso_3166_2=="DC",]$COORDS_X <- 2375000
usaMap[usaMap$iso_3166_2=="DC",]$COORDS_Y <- -500000

usaMap[usaMap$iso_3166_2=="MD",]$COORDS_X <- 2250000
usaMap[usaMap$iso_3166_2=="MD",]$COORDS_Y <- -600000

usaMap[usaMap$iso_3166_2=="DE",]$COORDS_X <- 2250000

usaMap[usaMap$iso_3166_2=="NJ",]$COORDS_X <- 2250000
usaMap[usaMap$iso_3166_2=="MA",]$COORDS_X <- 2540000

usaMap[usaMap$iso_3166_2=="NH",]$COORDS_X <- 2440000
usaMap[usaMap$iso_3166_2=="NH",]$COORDS_Y <- 220000

usaMap[usaMap$iso_3166_2=="VT",]$COORDS_X <- 2100000
usaMap[usaMap$iso_3166_2=="VT",]$COORDS_Y <- 480000

usaMap[usaMap$iso_3166_2=="HI",]$COORDS_X <- -330000
usaMap[usaMap$iso_3166_2=="HI",]$COORDS_Y <- -2200000

# should value labels be included in addition to the state labels?
labelValues <- FALSE

# state labels with values need to be slightly adjusted left/right as their centroids are imprecise
labelsToAdjustLeft <- c("TN", "KY", "NC", "WV")
labelsToAdjustRight <- c("RI", "CT", "DC", "MD", "DE","NJ", "MA", "NH", "TN", "KY", "NC")

# if value labels are included, adjust; if not, create state labels as is
# currently this code uses the fips_state variable to fill the value; change this to whatever variable you want to use as the label
if(labelValues) {
  usaMap$stateLabels <- ifelse(usaMap$iso_3166_2 %in% c(labelsToAdjustLeft, labelsToAdjustRight), paste0(usaMap$iso_3166_2, " (", usaMap$fips_state, ") "),
                    ifelse(usaMap$iso_3166_2 == "VT", paste0(usaMap$iso_3166_2,"\n(", usaMap$fips_state, ")", "\n"), paste0(usaMap$iso_3166_2, "\n(", usaMap$fips_state, ")")))
  usaMap$COORDS_X <- ifelse(usaMap$iso_3166_2 %in% labelsToAdjustRight, usaMap$COORDS_X + 80000, usaMap$COORDS_X)
  usaMap$COORDS_X <- ifelse(usaMap$iso_3166_2 %in% labelsToAdjustLeft, usaMap$COORDS_X - 17500, usaMap$COORDS_X)
} else {
  usaMap$stateLabels <- usaMap$iso_3166_2
}

# simplify the state borders to reduce file size and remove wild edges
usaMap <- st_simplify(usaMap, preserveTopology = TRUE, dTolerance = 1000)

# create a square polygon in the map where we can draw and fill DC and bind it to the 
usaMapDC <- usaMap[6,]
usaMap <- usaMap[-6,]
usaMapDC$geometry[[1]][1] <- st_polygon(list(matrix(c(c(2200000,2200000,2270000,2270000,2200000), c(-470000,-540000,-540000,-470000,-470000)), nrow = 5, ncol = 2)))
usaMap <- rbind(usaMap,usaMapDC)

### Adding in territories
{
## Virgin Islands
us_statesVI <- us_states[35,]
us_statesVI$geometry <- st_transform(us_statesVI$geometry, crs = "+proj=laea +x_0=-732500 +y_0=1210000 +lon_0=-74 +lat_0=40")*2.5
usaMapVI <- usaMap[6,]
usaMapVI$geometry[[1]] <- us_statesVI$geometry[[1]]
usaMapVI$name <- 'U.S. Virgin Islands'
usaMapVI$iso_3166_2 <- 'VI'
usaMapVI$COORDS_X <- 635000
usaMapVI$COORDS_Y <- -2700000
usaMapVI$stateLabels <- 'VI' 
usaMap <- rbind(usaMap,usaMapVI)

## American Samoa
us_statesAS <- us_states[42, ]
us_statesAS$geometry <- st_transform(us_statesAS$geometry, crs = "+proj=laea +x_0=+1375000 +y_0=2185000 +lon_0=-877 +lat_0=11")*5
usaMapAS <- usaMap[6,]
usaMapAS$geometry[[1]]<- us_statesAS$geometry[[1]]
usaMapAS$geometry[[1]][[2]]<- NULL
usaMapAS$geometry[[1]][[3]]<- NULL
usaMapAS$geometry[[1]][[4]]<- NULL
usaMapAS$geometry[[1]][[2]]<- NULL
usaMapAS$name <- 'American Samoa'
usaMapAS$iso_3166_2 <- 'AS'
usaMapAS$COORDS_X <- -690000
usaMapAS$COORDS_Y <- -2700000
usaMapAS$stateLabels <- 'AS' 
usaMap <- rbind(usaMap,usaMapAS)

## Puerto Rico
us_statesPR <- us_states[50, ]
us_statesPR$geometry <- st_transform(us_statesPR$geometry, crs = "+proj=laea +x_0=-635000 +y_0=75000 +lon_0=-74 +lat_0=40")*1.25
usaMapPR <- usaMap[6,]
usaMapPR$geometry[[1]] <- us_statesPR$geometry[[1]]
usaMapPR$name <- 'Puerto Rico'
usaMapPR$COORDS_X <- 200000
usaMapPR$COORDS_Y <- -2700000
usaMapPR$iso_3166_2 <- 'PR'
usaMapPR$stateLabels <- 'PR' 
usaMap <- rbind(usaMap,usaMapPR)

## Guam
us_statesGU <- us_states[37, ]
us_statesGU$geometry <- st_transform(us_statesGU$geometry, crs = "+proj=laea +x_0=1610000 +y_0=1935000 +lon_0=-74000 +lat_0=41")*3
usaMapGU <- usaMap[6,]
usaMapGU$geometry[[1]] <- us_statesGU$geometry[[1]]
usaMapGU$name <- 'Guam'
usaMapGU$COORDS_X <- -230000
usaMapGU$COORDS_Y <- -2700000
usaMapGU$iso_3166_2 <- 'GU'
usaMapGU$stateLabels <- 'GU' 
usaMap <- rbind(usaMap,usaMapGU)

## BIE
usaMapBIE <- usaMap[6,]
usaMapBIE$geometry[[1]][1] <- st_polygon(list(matrix(c(c(1000000,1000000,1110000,1110000,1000000), c(-2825000,-2925000,-2925000,-2825000,-2825000)), nrow = 5, ncol = 2)))   #yl: smart!
usaMapBIE$name <- 'Bureau of Indian Education'
usaMapBIE$COORDS_X <- 1050000
usaMapBIE$COORDS_Y <- -2700000
usaMapBIE$iso_3166_2 <- 'BIE'
usaMapBIE$stateLabels <- 'BIE' 
usaMap <- rbind(usaMap,usaMapBIE)
}


#read in the excel or csv file
if (str_sub(data,-4,-1) == 'xlsx') {
  state_data_4columns <- read_excel(data) 
} else {
    state_data_4columns <- read.csv(data)
  }

# standardize column names
colnames(state_data_4columns)[1:4] <- c("State", "Value", "Label","Legend")

# Trim whitespace of "State" columns
state_data_4columns$State <- trimws(state_data_4columns$State, which = c("both"))

#Adding hyphen for missing values
state_data_4columns <- state_data_4columns %>%
  mutate(Label = if_else(is.na(Label), '-', Label))

#select the id and value columns and make them factors to create binned categories
state_data <- state_data_4columns %>% 
  dplyr::select(State, Value, Label) %>% 
  mutate(State = as.factor(State),
         value = as.factor(Value),
         Label = as.factor(Label))

# join our figure data onto the geographic map data by state id 
usaMapTemp <- left_join(usaMap, state_data, by = c("name" = "State"))

# Adding margins
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}
GeomSf$draw_key = draw_key_polygon3

# create our color map palette
num_values <- length(unique(state_data$Value))

# In case missing values get read in as ""
state_data_4columns[state_data_4columns==""]<-NA

#select the legend cells in column 4 and remove NAs
state_legend <- state_data_4columns %>% 
  dplyr::select("Legend") %>%
  
  na.omit() 

print(state_legend)

# for the legend values, get rid of the string in the brackets and the things before " ="
state_legend <- gsub("^.+=\\s","",as.character(state_legend$'Legend'))

# change the font color for states that are adjusted outside of their state border (i.e. in the margins)
usaMapTemp$fontColor <- ifelse(usaMapTemp$iso_3166_2 %in% c("RI", "CT", "DC", "MD", "DE", "NJ", "MA", "NH", "VT","HI", 'AS', 'GU', 'PR', 'VI', "BIE"), as.character(unique(usaMapTemp$value)[1]), usaMapTemp$value)

#rename factor levels in usaMapTemp$value (so that the legend levels could be customized)
usaMapTemp$value <- factor(usaMapTemp$value, levels = levels(usaMapTemp$value), labels = state_legend)

# Get State colors/labels, depending how many labels there are
if (length(state_legend) == 5) {
  cols = c("#547DC3", "#D1DCEF", '#FEEFD8', '#FBB03B')
  label_cols = c('white', 'black', 'black', 'black') 
} else if (length(state_legend) == 6) {
    cols = c("#071D49", "#547DC3", "#D1DCEF", '#FEEFD8', '#FBB03B')
    label_cols = c('white', 'white', 'black', 'black', 'black')
} else if (length(state_legend) == 7) {
  cols = c("#071D49", "#547DC3", "#D1DCEF", '#FEEFD8', '#FBB03B', '#9C6D23')
  label_cols = c('white', 'white', 'black', 'black', 'black', 'white')
}

# Reverse the scale if user says so
if (rev_scale == TRUE) {
  cols = rev(cols)
  label_cols = rev(label_cols)
}

# for labels not within states
label_cols = append(label_cols, 'black')
label_cols = append(label_cols, 'black')
cols = append(cols, '#9aa9b2')


# Making the map theme:
theme_map <- function(...) {
  theme_void() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_blank(),
      legend.key = element_rect(fill = "transparent"),
      panel.border = element_blank(),
      ...
    )
}
# Plot the map 
gg <- ggplot(usaMapTemp) + geom_sf(aes(fill = value), color = '#576F7F', size= .21) +
  # add our map theme
  theme_map() + 
  # add additional font and custom theming (this may need some tweeking depending on the results)
  theme(text = element_text(family = "Museo Slab 500", size = 12),panel.grid.major = element_line(colour = 'transparent'),
        legend.background = element_rect(fill="#FFFFFF", color = "transparent"),
        legend.margin=margin(t = .25,l = .25,b = .25,r = .25, unit='cm'),
        
        legend.title = element_text(family = "Museo Slab 700", size = 10),
        legend.text = element_text(family = "Museo Slab 500", size = 10), 
        legend.key = element_rect(size = 1, color = "#FFFFFF"),
        legend.position = c(.88, 0.125),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  # add fill color scale for states
  scale_fill_manual(na.value = "#9aa9b2", values = cols) +
  
  # position the state labels at x/y coordinates using custom font
  geom_text(mapping = aes(COORDS_X, COORDS_Y+1000, label = stateLabels, family = "Museo Slab 500", color = fontColor), size = 3.5, show.legend = FALSE) +
  geom_text(mapping = aes(COORDS_X, COORDS_Y-55000, label = Label, family = "Museo Slab 500", color = fontColor), size = 3, show.legend = FALSE) +
  
  # map color aesthetic for state labels
  scale_color_manual(na.value = "black", values=label_cols) + 
  
  # legend customization
  guides(fill = guide_legend(
    override.aes = list(size = .25),
    title="Legend"
    )
  )

# add lines for state labels that are adjusted far away from the state location
gg <- gg + 
  annotate("segment", x = 1990000, xend = 2170000, y = -440000, yend =  -550000, colour = "black") + # MD
  annotate("segment", x = 2310000, xend = 2550000, y = -100, yend =  -35000, colour = "black") + # RI
  annotate("segment", x = 2200000, xend = 2430000, y = -60000, yend =  -150000, colour = "black") + # CT
  annotate("segment", x = 1950000, xend = 2197000, y = -400000, yend =  -500000, colour = "black") + # DC
  annotate("segment", x = 2200000, xend = 2080000, y = -370000, yend =  -340000, colour = "black") + # DE
  annotate("segment", x = 2200000, xend = 2100000, y = -221000, yend =  -200000, colour = "black") + # NJ
  annotate("segment", x = 2320000, xend = 2480000, y = 130000, yend =  80000, colour = "black") + # NJ
  annotate("segment", x = 2300000, xend = 2375000, y = 200000, yend =  210000, colour = "black") + # NH
  annotate("segment", x = 2125000, xend = 2115000, y = 300000, yend =  400000, colour = "black") + # VT
  
  ### Divider lines
  annotate("segment", x = 850000, xend = 850000, y = -3100000, yend =  -2600000, colour = "#576F7F") +
  annotate("segment", x = 420000, xend = 420000, y = -3100000, yend =  -2600000, colour = "#576F7F") +
  annotate("segment", x = -15000, xend = -15000, y = -3100000, yend =  -2600000, colour = "#576F7F") +
  annotate("segment", x = -450000, xend = -450000, y = -3100000, yend =  -2600000, colour = "#576F7F") 

ggg <- ggplotGrob(gg)

### Saving the plot
if (output == 'eps') {
# load font again
loadfonts(device = "postscript", quiet = TRUE)
# save as eps
setEPS()
postscript(paste0(sub('\\..*', '', data), ".eps"), fonts =c("Museo Slab 500", "Museo Slab 700") ,width = 13.84, height = 7.86) #width and height are in inches
grid::grid.draw(ggg)
dev.off()
} else {
  ggsave(filename = paste0(sub('\\..*', '', data), ".", output),width = 13.84, height = 7.86)
}

plot(ggg)
