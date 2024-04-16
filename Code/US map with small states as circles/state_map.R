# [10] Slide 20 state map ####
setwd("C:/Users/yliao/Downloads/")

library(sf)
library(tidyverse)
library(readxl)
library(tigris)
library(cowplot)
library(gganimate)
library(extrafont)
library(sysfonts)
library(magick)

# leaver color palette (yellow)
y5 = "#AE7023"
y4 = "#CE8B2C"
y3 = "#EDA833"
y2 = "#FACD61"
y1 = "#F8F0D9"

# mover color palette (green)
g5 = "#008624"
g4 = "#5CA22A"
g3 = "#93BD41"
g2 = "#C7D872"
g1 = "#EFF1DF"

# load shapefile and prep dfs
us0 <- st_read("C:/Users/yliao/Downloads/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")

# [0] Load fonts ####
font_add(family = "Publico Text", regular = "C:/Users/yliao/Downloads/publico/PublicoText-Roman.otf",
         bold = "C:/Users/yliao/Downloads/publico/Publico-Bold.otf",
         italic = "C:/Users/yliao/Downloads/publico/Publico-Italic.otf",
         bolditalic = "C:/Users/yliao/Downloads/publico/Publico-BoldItalic.otf")

loadfonts()

# load data ####
dfs <- map(set_names(excel_sheets(paste0("data_for_R.xlsx"))),
           read_excel, path = paste0("data_for_R.xlsx")
)

df_m <- dfs$state_maps %>%
  select(state,movers,movers_val)
quantile(df_m$movers, probs = seq(0, 1, 1/5),na.rm = T)  

df_l <- dfs$state_maps %>%
  select(state,leavers,leavers_val)
quantile(df_l$leavers, probs = seq(0, 1, 1/5),na.rm = T) 

us <- us0 %>%
  filter(STATEFP < 60) %>%
  shift_geometry()

# merge and set legend buckets
movers <- merge(us,df_m,by.x = "NAME",by.y = "state",all = T) %>%
  mutate(leg = case_when(movers >= 2.4 & movers < 5.6 ~ "2.4-5.5  ", 
                         movers >=5.6 & movers < 6.6 ~ "5.6-6.5  ",
                         movers >=6.6 & movers < 8 ~ "6.6–7.9  ",
                         movers >=8 & movers < 9.8 ~ "8.0–9.7  ",
                         movers >= 9.8 ~ "9.8–41.4  ",
                         is.na(movers)~"‡"),
         leg = factor(leg, levels = c("2.4-5.5  ","5.6-6.5  ","6.6–7.9  ","8.0–9.7  ","9.8–41.4  ","‡")),
         movers_val = ifelse(movers_val == "9.8000000000000007","9.8",movers_val),
         st_lab = paste0(STUSPS,"\n",movers_val))

leavers <- merge(us,df_l,by.x = "NAME",by.y = "state",all = T) %>%
  mutate(leg = case_when(leavers >= 3.5 & leavers < 5.2 ~ "3.5-5.1  ", 
                               leavers >=5.2 & leavers < 7.0 ~ "5.2-6.9  ",
                               leavers >=7.0 & leavers < 7.7 ~ "7.0–7.6  ",
                               leavers >=7.7 & leavers < 10.4 ~ "7.7–10.3  ",
                               leavers >= 10.4 ~ "10.4–24.5  ",
                         is.na(leavers) ~"‡"),
         leg = factor(leg, levels = c("3.5-5.1  ","5.2-6.9  ","7.0–7.6  ","7.7–10.3  ","10.4–24.5  ","‡")),
         st_lab = paste0(STUSPS,"\n",leavers_val))

fs2 <- 22

# set mapping function
map_f <- function(df,title,clrs,alpha){
  # create circles for small states
  ex_dat <- df %>%
    filter(STUSPS %in% c("VT","NH","MA","RI","CT",
                         "NJ","DE","MD","DC")) %>%
    mutate(STUSPS = factor(STUSPS,levels = c("VT","NH","MA","RI","CT",
                                             "NJ","DE","MD","DC"))) %>%
    arrange(STUSPS) %>%
    mutate(x = c(1,1.5,1,1.5,1,1.5,1,1.5,1),
           y = c(5,5,4,4,3,3,2,2,1))
  
  ex_plot <- ggplot(ex_dat,aes(x = x,y = y,
                               label = st_lab)) +
    geom_point(color = "grey30",size = 14) +
    geom_point(color = "white",size = 13) +
    geom_point(aes(color = leg),size = 13, alpha=alpha) +
    geom_text(lineheight = .9, family = "Publico Text",
              size = 3.5) +
    scale_x_continuous(limits = c(.5,2)) +
    scale_y_continuous(limits = c(0,6)) +
    scale_color_manual(values = clrs, drop = F,na.value = "grey70") +
    theme_void() +
    theme(legend.position = "none")
  
  # main map
  map_plot <- ggplot() +
    geom_sf(data = df, 
            mapping = aes(fill = leg), alpha=alpha,
            color = "grey30",linewidth = .1) +
    geom_sf_text(data = df %>%
                   filter(!STUSPS %in% c("VT","NH","MA","RI","CT",
                                         "NJ","DE","MD","DC")), 
                 aes(label = st_lab),lineheight = .9,
                 family = "Publico Text",size = 3.5) +
    scale_fill_manual(values = clrs,na.value = "grey70") +
    labs(fill = paste0("Percent ",title, " (by quintile):")) +
    theme_void()+
    theme(legend.position = "bottom",
          legend.text = element_text(size = fs2-9, family = "Publico Text"),
          legend.title = element_text(size = fs2-9,family = "Publico Text",
                                      face = "bold"),
          legend.box.margin=margin(t=25),
          legend.margin = margin(l=50,t=-30)) +
    guides(fill = guide_legend(nrow = 1,
                               title.position = "top"))
  
  # paste eastern states onto main map
  ggdraw(map_plot) +
    draw_plot(ex_plot,
              x=.94,y = .38,
              width = .2,height = .5) +
    theme(plot.margin = margin(r=65))
}

alpha <- 1
for (step in seq(0,5,1)){
  print(paste("working on step", step))
  if (step == 0){
    map_f(leavers,"leavers",c("white","white","white","white","white","grey70"),alpha)
    ggsave(paste0("./mover_alpha/", step, "_", "alpha_",sprintf("%.2f", alpha),"_s20a.png"), 
           width = 10, height = 8.5, dpi = 800,bg = "white")
  }
  if (step == 1){
    map_f(leavers,"leavers",c(y1,"white","white","white","white","grey70"),alpha)
    ggsave(paste0("./mover_alpha/", step, "_", "alpha_",sprintf("%.2f", alpha),"_s20a.png"), 
           width = 10, height = 8.5, dpi = 800,bg = "white")
  }
  if (step == 2){
    map_f(leavers,"leavers",c(y1,y2,"white","white","white","grey70"),alpha)
    ggsave(paste0("./mover_alpha/", step, "_", "alpha_",sprintf("%.2f", alpha),"_s20a.png"), 
           width = 10, height = 8.5, dpi = 800,bg = "white")
  }
  if (step == 3){
    map_f(leavers,"leavers",c(y1,y2,y3,"white","white","grey70"),alpha)
    ggsave(paste0("./mover_alpha/", step, "_", "alpha_",sprintf("%.2f", alpha),"_s20a.png"), 
           width = 10, height = 8.5, dpi = 800,bg = "white")
  }
  if (step == 4){
    map_f(leavers,"leavers",c(y1,y2,y3,y4,"white","grey70"),alpha)
    ggsave(paste0("./mover_alpha/", step, "_", "alpha_",sprintf("%.2f", alpha),"_s20a.png"), 
           width = 10, height = 8.5, dpi = 800,bg = "white")
  }
  if (step == 5){
    map_f(leavers,"leavers",c(y1,y2,y3,y4,y5,"grey70"),alpha)
    ggsave(paste0("./mover_alpha/", step, "_", "alpha_",sprintf("%.2f", alpha),"_s20a.png"), 
           width = 10, height = 8.5, dpi = 800,bg = "white")
  }
  
}



images <- list.files(path='./mover_alpha/', pattern = '*.png', full.names = TRUE) 
img <- image_read(images)
img <- image_join(img)
img_animate <- image_animate(img, fps=100)
image_write(img_animate, "mover_gif.gif")

# map_f(movers,"movers",c(g1,g2,g3,g4,g5,"grey70"))
# ggsave(paste0(homepath,"s20b.png"), 
#        width = 10, height = 8.5, dpi = 800,bg = "white")