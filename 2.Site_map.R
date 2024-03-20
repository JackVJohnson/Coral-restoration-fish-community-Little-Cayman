########################################################################################
################################# GENERIC LC MAP #######################################
########################################################################################

# objective: Map  
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: September 2023
# Last edited: 

# clear workspace
rm(list=ls())

# libraries

library(tidyverse)
library(fishualize)
library(patchwork)
library(sf)
library(ggmap)
library(ggsn)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

#########################################################################################
################################## working directories ##################################

shapefile_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Shapefiles/Cayman Islands"

output_wd <-  "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/Resembid_fish/Output_directory"

my_theme <- theme_classic() +
  theme(axis.title.x = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black", angle = 0, vjust = .0)) +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold")) +
  theme(plot.title = element_text(size = 22, face = "bold"))

#########################################################################################
################################## read in shape file  ##################################

setwd(shapefile_wd)
#Little Cayman
#Longitude: -80.133056°, Latitude: 19.730000°
#Longitude: -79.936944°, Latitude: 19.626389°

# site 
#N 19°41'59" = 19.699722° (latitude)
#W 80°03'42" = -80.061667° (longitude)
site_coordinates <- data.frame(Longitude = -80.061667, Latitude = 19.699722)


gc_map <- st_read("cym_admbnda_adm0_2020.shp")
gc_map
gc_map <- st_transform(gc_map, crs = 4326)

p1 <- ggplot() +
  geom_sf(data=gc_map, fill = "white") +
  coord_sf(xlim=c(-80.21056,-79.936944), ylim=c(19.62638, 19.790000), expand = T) +
  geom_point(data = site_coordinates, aes(x = Longitude, y = Latitude), shape = 16, size = 5, color = "black") +
  annotation_scale(location = "tr",
                         pad_x = unit(0.5, "cm"), pad_y = unit(13, "cm")) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(1.5, "cm"), pad_y = unit(11, "cm") ,
                         height = unit(2, "cm"), width = unit(2, "cm") ,
                         style = north_arrow_fancy_orienteering) +
  theme_classic() +
  xlab("") + ylab("") +
  theme(axis.title.x = element_text(size = 22, color = "black"), axis.title.y = element_text(size = 22, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black", angle = 0, vjust = .0)) +
  theme(legend.position = "none")  +
  theme(panel.background = element_rect(fill = "#5ECFFA")) +
  theme(panel.border= element_rect(colour = "black", linewidth =1, fill =NA))

p1  

# trim a world map to show the wider Caribbean region 

ocean <- ne_download(type = "ocean", category = "physical", scale = "medium")
ocean <- st_as_sf(ocean)

p2 <- ggplot() + 
  geom_sf(data = ocean, fill = "#5ECFFA") +
  coord_sf(xlim=c(-92.478113,-62.031176), ylim=c(8.8,27), expand = T) +
  annotation_scale() +
  #annotation_north_arrow(location = "tr", which_north = "true", 
  #                       height = unit(1, "cm"), width = unit(1, "cm") ,
  #                       style = north_arrow_fancy_orienteering) +
  theme_classic() +
  xlab("") + ylab("") +
  #theme(panel.background = element_rect(fill = "white")) +
  geom_rect(aes(xmin = -80.5, xmax = -79.5, ymin = 19, ymax = 20), color = "black", fill = NA, linewidth = 1) +
  annotate("text", x=-78, y=15, label = "Caribbean 
Sea", size = 8) +
  theme(axis.title.x = element_text(size = 22, color = "black"), axis.title.y = element_text(size = 22, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black", angle = 0, vjust = .0)) +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", linewidth =1, fill =NA),
    axis.text.x=element_blank(), 
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank()
  )
  
  
  
  
  theme(panel.border= element_rect(colour = "black", linewidth =1, fill ="#5ECFFA")) +
  theme(panel.background = element_blank()) +
  theme(a) 


#theme(plot.background = element_rect(colour = "black", linewidth =2)) 
#theme(
#  panel.background = element_rect(fill='transparent'), #transparent panel bg
#  plot.background = element_rect(fill='transparent'))

p2

#########################################################################################
########################## combine plots using patchwork ################################
p1


#png(file=file.path(output_wd, "Map_FIGURE.png"), height = 4000, width = 5000, res = 350)
#(p1 + p2 + plot_layout(widths = c(2, 1))) + plot_annotation(tag_levels = 'A') &
#  theme(plot.tag = element_text(size = 30))
#dev.off()

#png(file=file.path(output_wd, "Map_FIGURE.png"), height = 4000, width = 4000, res = 400)
#p1 + inset_element(p2, left = 0.07, bottom = 0.6, right = 0.4, top = 0.99, align_to = 'full')
#dev.off()


tiff(file=file.path(output_wd, "Map_FIGURE.tif"), height = 4000, width = 4000, res = 400)
p1 + inset_element(p2, left = 0.035, bottom = 0.5, right = 0.6, top = 1, align_to = "full")
dev.off()







