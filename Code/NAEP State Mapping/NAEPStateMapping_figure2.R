### NAEP State Mapping Figure
### 1/29/19
### Yuqi Liao, Michael Lee, & Howard Huo



### Setting things up -----
reqpkg <- c("dplyr", "ggplot2", "readxl", "grid", "grDevices", "showtext", "extrafont")

sapply(reqpkg, function(pkgi) {
  if (!pkgi %in% installed.packages()) {
    install.packages(pkgi)
  }
  library(pkgi, character.only = TRUE)
})

wd <- "G:/Data Science/GIT/InternationalAssessment_DataViz/Code/NAEP State Mapping"

setwd(wd)

### Reading in data -----
data <- read_excel("./Materials/G4ReadingMiddle_yl.xlsx") %>% 
  mutate(yearCol = ifelse(year %in% 2017, "#212C68", ifelse(year %in% 2015, "#C8942B", "#8E9FBC") ))

# define order of proficiencyLevel and year
orderProficiencyLevel <- unique(data$proficiencyLevel)
data$proficiencyLevel <- factor(data$proficiencyLevel, levels = rev(orderProficiencyLevel))

orderYear <- unique(data$year)
data$year <- factor(data$year, levels = orderYear,
                    labels = c(" 2007", " 2015", " 2017"))

glimpse(data)
### Plotting -----

# define a few settings needed for plotting
yAxisFloor <- 0
yAxisCeiling <- 50
yAxisBreaks <- seq(from = yAxisFloor, to = yAxisCeiling, by = 10)
cols <- c("#8E9FBC", "#C8942B", "#212C68")


# load font
font_add_google("Open Sans")
showtext_auto()

# define theme_general
theme_general <- theme(text=element_text(family="Open Sans", color = "#000000"),
                       panel.background=element_blank(),
                       panel.border=element_rect(color="transparent"),
                       plot.margin = unit(c(0,0.02,0,0), "npc"),
                       panel.grid.major.y=element_blank(),
                       panel.grid.major.x=element_blank(),
                       panel.grid.minor.x=element_blank(),
                       panel.grid.minor.y=element_blank(),
                       axis.title = element_text(family="Open Sans", color = "#000000", 
                                                 size = 10, face = "bold"),
                       axis.line.x=element_line(color="#2d2a26", size = 0.235),
                       axis.line.y=element_line(color="#2d2a26", size = 0.235),
                       axis.text.x=element_text(color="#000000", size= 10, margin = margin(t = 3)),
                       axis.text.y=element_text(color="#000000", size= 10, face = "italic",
                                                margin = margin(r = -2)),
                       axis.ticks.y=element_blank(),
                       axis.ticks.x=element_line(color="#2d2a26", size= 0.235),
                       axis.ticks.length = unit(7.3,"points"),
                       plot.title=element_text(family="Open Sans", size= 10 ,lineheight=2, 
                                               color="#000000", face = "bold"),
                       legend.title = element_blank(),
                       #legend.spacing.y = unit(1, "cm"), #not working
                       legend.key.height=unit(0.7,"line"),
                       legend.key.width=unit(0.7,"line"),
                       #legend.key.size = unit(2, "cm"),
                       legend.text = element_text(size=10, family = "Open Sans"),
                       legend.position = c(0.9, 0.85),
                       legend.box.background = element_blank()
                       #legend.box.margin = margin(0,0,0,0)
                       #aspect.ratio = 3.718/7.4328
                       )

# add margins to legend squares [reference: https://github.com/tidyverse/ggplot2/issues/2844]
draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.7, "npc"),
    height = grid::unit(0.7, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}
GeomCol$draw_key = draw_key_polygon3
# end margin code


# plotting
plot <- ggplot(data, aes(x = proficiencyLevel, y = number, fill = year, label = year)) +
 
  # add grouped bars
  geom_col(width = 0.7, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = cols, 
                    breaks = rev(levels(data$year))) +
  
  coord_flip(clip = "off") +
  
  # adjust the expand if needed (default to be c(0.2, 0.2))
  #scale_x_discrete(expand = c(0.2, 0.2)) +
  
  scale_y_continuous(limits = c(yAxisFloor, yAxisCeiling), 
                     labels = yAxisBreaks, 
                     breaks = yAxisBreaks, expand = c(0,0)) +
  
  # add text/label
  geom_text(colour = rev(data$yearCol), size = 3.5, position = position_dodge(width = 0.8), hjust = -0.1) +
  
  # apply themes 
  theme_bw() + 
  theme_general +
  
  # add axis titles
  labs(x = "", y = "Number of states", title = "NAEP achievement level")


# move title to the left
grid.newpage()
g <- ggplotGrob(plot)
g$layout$l[g$layout$name == "title"] <- 4
grid::grid.draw(g)


### Exporting -----

# # use ggsave() - worked in the PLS example, didn't work this time (text outline)
# library(extrafont)
# loadfonts(device = "win")
# ggsave(paste0("./Results/", "ex6", ".eps"), width = 5.61 * 2, height = 2.81 * 2, dpi = 300, family = "Open Sans")
#   
#   
# # previous code for eps output, though the text appears as image not text
# cairo_ps(paste0("./Results/", "ex7", ".eps"), width = 13.84, height = 7.86)
# grid::grid.draw(g)
# dev.off()

#postscript() - didn't work in the PLS example, but work this time

# load font again
loadfonts(device = "postscript")
# save as
setEPS()
postscript(paste0("./Results/", "figure2_ex3", ".eps"), family = "Open Sans", width = 3.8, height = 2.1025) #width and height are in inches
grid::grid.draw(g)
dev.off()

