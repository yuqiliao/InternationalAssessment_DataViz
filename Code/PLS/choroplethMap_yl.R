# Install packages
if (!require(rgeos)) {
  install.packages("rgeos")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal")
  require(rgdal)
}
if (!require(raster)) {
  install.packages("raster")
  require(raster)
}
if(!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(viridis)) {
  install.packages("viridis")
  require(viridis)
}
if(!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
if(!require(gtable)) {
  install.packages("gtable")
  require(gtable)
}
if(!require(grid)) {
  install.packages("grid")
  require(grid)
}
if(!require(readxl)) {
  install.packages("readxl")
  require(readxl)
}
if(!require(magrittr)) {
  install.packages("magrittr")
  require(magrittr)
}
if(!require(maptools)) {
  install.packages("maptools")
  require(maptools)
}

# if(!require(showtext)) {
#   install.packages("showtext")
#   require(showtext)
# }

wd <- "H:/ESSIN Pubs and Graphic Support/04_Project Working Files/Automating data figures/work with Yuqi/PLS/Map"

setwd(wd)

# Map theme
theme_map <- function(...) {
  theme_void() +
    theme(
      text = element_text(color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}



## using sf
library(albersusa) # devtools::install_github("hrbrmstr/albersusa")
library(sf)
library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(viridis)
library(scales)
library(purrr)
library(readxl)

figure_list <- c("fig 2-11", "fig1-3", "fig1-4", "fig2-12", "fig2-13", "fig2-14", 
                 "fig2-15", "fig2-16", "fig2-20", "fig2-21", "fig2-22", "fig2-3", 
                 "fig2-4", "fig2-5", "fig2-7", "fig2-8", "fig3-2", "fig3-3")

for (figure_name in figure_list){
  #howard's code
  #state_data <- read.csv(paste0("H:/ESSIN Pubs and Graphic Support/04_Project Working Files/Automating data figures/work with Howard/PLS/csv/", figure_name, ".csv"))
  
  #read in the excel file
  state_data_4columns <- read_excel(paste0("./Raw data/", figure_name, ".xlsx"))
  
  #select the id and value columns and make them factors
  state_data <- state_data_4columns %>% 
    select(id, value) %>% 
    mutate(id = as.factor(id),
           value = as.factor(value))
  
  #select the legend cells in column 4
  state_legend <- state_data_4columns %>% 
    select(X__2) %>% 
    na.omit() 
  
  #get rid of the string in the brackets and the things before " ="
  state_legend <- gsub("\\s*\\([^\\)]+\\)","",as.character(state_legend$X__2))  
  state_legend <- gsub("^.+=\\s","",state_legend)
    
  
  # usa albers projection
  usa_sf <-
    st_as_sf(usa_composite("laea")) %>%
    mutate(
      CENTROID = map(geometry, st_centroid),
      COORDS = map(CENTROID, st_coordinates),
      COORDS_X = map_dbl(COORDS, 1),
      COORDS_Y = map_dbl(COORDS, 2)
    ) %>%
    as_tibble() %>%
    st_as_sf()
  
  x <- left_join(usa_sf, state_data,by = c("name" = "id"))
  z <- st_simplify(x, preserveTopology = TRUE,dTolerance = 1000)
  
  #  customize location of labels for small/odd state shapes
  z[z$iso_3166_2=="MI",]$COORDS_X <- 1245000
  z[z$iso_3166_2=="MI",]$COORDS_Y <- -100000
  
  z[z$iso_3166_2=="LA",]$COORDS_X <- 700000
  z[z$iso_3166_2=="LA",]$COORDS_Y <- -1450000
  
  z[z$iso_3166_2=="FL",]$COORDS_X <- 1825000
  z[z$iso_3166_2=="FL",]$COORDS_Y <- -1700000
  
  #  customize location of labels for small/odd state shapes
  z[z$iso_3166_2=="RI",]$COORDS_X <- 2600000
  z[z$iso_3166_2=="RI",]$COORDS_Y <- -30000
  
  z[z$iso_3166_2=="CT",]$COORDS_X <- 2700000
  z[z$iso_3166_2=="CT",]$COORDS_Y <- -110000
  
  z[z$iso_3166_2=="DC",]$COORDS_X <- 2350000
  z[z$iso_3166_2=="DC",]$COORDS_Y <- -500000
  
  z[z$iso_3166_2=="MD",]$COORDS_X <- 2250000
  z[z$iso_3166_2=="MD",]$COORDS_Y <- -600000
  
  z[z$iso_3166_2=="DE",]$COORDS_X <- 2250000
  z[z$iso_3166_2=="NJ",]$COORDS_X <- 2250000
  z[z$iso_3166_2=="MA",]$COORDS_X <- 2540000
  
  z[z$iso_3166_2=="NH",]$COORDS_X <- 2440000
  z[z$iso_3166_2=="NH",]$COORDS_Y <- 200000
  
  z[z$iso_3166_2=="VT",]$COORDS_X <- 2100000
  z[z$iso_3166_2=="VT",]$COORDS_Y <- 430000
  
  z[z$iso_3166_2=="HI",]$COORDS_X <- -330000
  z[z$iso_3166_2=="HI",]$COORDS_Y <- -2200000
  
  z$fontColor <- ifelse(z$iso_3166_2 %in% c("RI", "CT", "DC", "MD", "DE", "NJ", "MA", "NH", "VT","HI"), as.character(unique(z$value)[1]), z$value)
  
  
  zz2 <- z[6,]
  
  zz2$geometry[[1]][1] <- st_polygon(list(matrix(c(c(2200000,2200000,2270000,2270000,2200000), c(-470000,-540000,-540000,-470000,-470000)), nrow = 5, ncol = 2)))   #yl: smart!

  z <- rbind(z,zz2)
  
  # needed to add margins to legend squares
  #yl: reference: https://github.com/tidyverse/ggplot2/issues/2844
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
  # end margin code
  
  colfunc <- colorRampPalette(c("#00263e", "#ffffff"))
  num_values <- length(unique(state_data$value))
  
  #rename factor levels in z$value (so that the legend levels could be customized)
  z$value <- factor(z$value, levels = levels(z$value), labels = sort(state_legend))
  
  gg <- ggplot(z) +
    geom_sf(aes(fill = value), size= .75, color = "#36454f") + theme_map() + theme(text = element_text(size = 12),panel.grid.major = element_line(colour = 'transparent'),
                                                                                   legend.background = element_rect(fill="#f5f5f5", color = "transparent"),
                                                                                   legend.margin=margin(t = .5,l = .5,b = .5,r = .5, unit='cm'),
                                                                                   legend.text = element_text(size = 12), legend.key = element_rect(size = 1, color = "#f5f5f5"),
                                                                                   legend.position = c(1, 0.3),
                                                                                   plot.margin=grid::unit(c(0,50,0,-5), "mm")) +
    scale_fill_manual(na.value = "black",values = colfunc(num_values)) +
    geom_text(mapping = aes(COORDS_X, COORDS_Y, label = iso_3166_2, color = fontColor), size = 4.5, show.legend = FALSE) +
    scale_color_manual(values=c(rep("white", num_values%/%2), rep("black", num_values - num_values%/%2 + 1))) +
    guides(fill = guide_legend(
      override.aes = list(size = .5),
      title="Value"
    )
    )
  
  gg <- gg + 
    annotate("segment", x = 1990000, xend = 2170000, y = -440000, yend =  -550000, colour = "black") + # MD
    annotate("segment", x = 2310000, xend = 2550000, y = -100, yend =  -35000, colour = "black") + # RI
    annotate("segment", x = 2200000, xend = 2630000, y = -60000, yend =  -110000, colour = "black") + # CT
    annotate("segment", x = 1950000, xend = 2197000, y = -400000, yend =  -500000, colour = "black") # DC
  
  gg
  
  #previous code for eps output, though the text appears as image not text
  # cairo_ps(paste0("./results/", figure_name, ".eps"), width = 13.84, height = 7.86)
  # print(gg)
  # dev.off()
  
  #postscript() and ggsave() below works for eps
  # setEPS()
  # postscript(paste0("./results/", "postscript", ".eps"), family = "ArialMT", width = 13.84, height = 7.86 )
  # print(gg)
  # dev.off()
  
  
  ggsave(paste0("./results/", figure_name, ".eps"), width = 13.84, height = 7.86, dpi = 300, family = "ArialMT")
  ggsave(paste0("./results/", figure_name, ".png"), width = 13.84, height = 7.86, dpi = 96)
  cairo_pdf(paste0("./results/", figure_name, ".pdf"), width = 13.84, height = 7.86)
  print(gg)
  dev.off()
}