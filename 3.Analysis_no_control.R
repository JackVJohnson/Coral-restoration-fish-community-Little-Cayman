########################################################################################
################################## RESEMBID fish #######################################
########################################################################################

# objective: Data exploration CONTROL REMOVED 
# Author: Jack Johnson (jackvjohnson@hotmail.com)
# Date created: September 2023
# Last edited: 

# clear workspace
rm(list=ls())

# libraries

library(tidyverse)
library(fishualize)
library(patchwork)
library(FSA)
library(vegan)

#########################################################################################
################################## working directories ##################################

data_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/Resembid_fish/Data_files"

shapefile_wd <- "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Shapefiles/Cayman Islands"

output_wd <-  "C:/Users/jackv/OneDrive - Central Caribbean Marine Institute, Inc/Projects/Resembid_fish/Output_directory"

my_theme <- theme_classic() +
  theme(axis.title.x = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20, color = "black"), text=element_text(size=16)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16, color="black", angle = 0, vjust = .0)) +
  theme(legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold")) +
  theme(plot.title = element_text(size = 22, face = "bold"))

##########################################################################################
##################################### read in files ######################################

April_fish <- read.csv(file=file.path(data_wd, "Resembid_Fish_April23_Master.csv"))
June_fish <- read.csv(file=file.path(data_wd, "Resembid_Fish_June23_Master.csv"))

fishualize("Clepticus_parrae", n=6)
fishualize::fish(option="Clepticus_parrae", n=6)

# combine the dataframes

fish_df <- rbind(April_fish, June_fish)
fish_df <- fish_df[,0:18]
# careful, some depths are in ft, some in meters, some have been dragged so are wrong, and JVJ didn't fill his out
# dome depth
#   1    60
#   2    70
#   3    60 
#   4    60
#   5    60 
#   C    65

# remove depth and day because some have been dragged down in excel so are wrong. Day can be covered by date and correct depths for each dome above
fish_df <- fish_df[,-c(4:5)]


table(fish_df$sizeclass)

table(fish_df$Dome)

summary(fish_df)
rm(April_fish, June_fish)

fish_df$biomass <- as.numeric(fish_df$biomass)
summary(fish_df$biomass)

# messy messy data 
fish_df$Dome <- gsub("control", "Control", fish_df$Dome)

# vector of time since sampling. 8th of April only for controls so counts as day zero 
fish_df <- fish_df %>%
  mutate(outplant_time = ifelse(date == "06-Apr-23", "Before",
                                ifelse(date == "08-Apr-23", "Before",
                                       ifelse(date == "11-Apr-23", "Day 5",
                                              ifelse(date == "30-Jun-23", "Day 85", NA)))))


# domes are replicates

library(dplyr)

fish_df <- fish_df %>%
  mutate(Treatment = ifelse(Dome %in% c(1, 2, 3, 4, 5), "Outplants", "Control"))

fish_df <- na.omit(fish_df)

fish_df$Treatment <- as.factor(fish_df$Treatment)

fish_df <- subset(fish_df, Treatment == "Outplants")


sum_df <- fish_df %>%
  group_by(date, month, observer, Dome, Treatment, outplant_time) %>%
  summarise(sum_biomass = sum(biomass),
            sum_abundance = sum(count),
            sum_richness = n_distinct(fish_spp))

df <- sum_df %>%
  group_by(date, month, Dome, Treatment, outplant_time) %>%
  summarise(mean_biomass = mean(sum_biomass),
            mean_abundance = mean(sum_abundance),
            mean_richness = mean(sum_richness))


unique(df$date)

#################################################################################
############################# check size classes ################################


table(fish_df$sizeclass)

fish_df <- fish_df %>%
  mutate(size = case_when(
    sizeclass %in% c(5, 10) ~ "small", # small sizes
    TRUE ~ "big"                      # all other sizes
  ))
table(fish_df$size)

size_df <- fish_df %>%
  group_by(date, month, observer, Dome, Treatment, outplant_time, size) %>%
  summarise(mean_abundance = sum(count)/length(unique(Dome)))

ggplot(size_df, aes(outplant_time, mean_abundance, fill = as.factor(size))) +
  geom_boxplot() +
  my_theme


#################################################################################

p1 <- ggplot(df, aes(outplant_time, mean_biomass/1000)) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  #coord_cartesian(ylim =  c(0, 25))  +
 # scale_fill_fish_d("Clepticus_parrae", begin = 0.2, end =1) +
  #scale_fill_manual(values=c("#CD6A9BFF", "#0646CFFF")) +
  xlab("") +
  ylab("Mean biomass (kg)") +
  labs(title = "Fish communtiy") +
  my_theme 
p1


# Define the base color
base_color <- "#CD6A9B"

# Generate different hues by adjusting saturation and brightness
hues <- c(30, 60, 90)  # Adjust these values as needed
colors <- paste0(base_color, hues)

# Create the ggplot with different hues
p1 <- ggplot(df, aes(x = outplant_time, y = mean_biomass/1000, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black", outlier.shape = NA) +
  geom_point(size=4) +
  scale_fill_manual(values = colors) + 
  xlab("") +
  ylab("Mean biomass (kg)") +
  labs(title = "Fish community") +
  my_theme +
  theme(legend.position = "none")

# Plot
p1

geom_text(aes(label = cld, y = w + sd), vjust = -0.5)

p2 <- ggplot(df, aes(outplant_time, mean_abundance, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") +
  geom_point(size=4) +
  #scale_fill_fish_d("Clepticus_parrae", begin = 0.2, end =1) +
  scale_fill_manual(values = colors) + 
  xlab("") +
  ylab("Mean abundance") +
  my_theme +
  theme(legend.position = "none")
p2

p3 <- ggplot(df, aes(outplant_time, mean_richness, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") + 
  scale_fill_manual(values = colors) + 
  geom_point(size=4) +
  #scale_fill_fish_d("Clepticus_parrae", begin = 0.2, end =1) +
  xlab("Outplant time") +
  ylab("Mean species richness") +
  my_theme +
  theme(legend.position = "none")
p3

png(file=file.path(output_wd, "summary_boxplots_NOCONTROL.png"), height = 4000, width = 2500, res = 350)
(p1/p2/p3) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 30))
dev.off()



# statistical analysis 

hist(df$mean_biomass)
hist(df$mean_abundance)
hist(df$mean_richness)

kruskal.test(mean_biomass ~ outplant_time, data = df)
kruskal.test(mean_abundance ~ outplant_time, data = df)
kruskal.test(mean_richness ~ outplant_time, data = df)


##########################################################################################
################################### check fish groups ####################################

rm(p1,p2,p3)

fishualize("Clepticus_parrae", n=4)
fishualize::fish(option="Clepticus_parrae", n=4)

# herbivores

h_base <- "#292360"
h_colors <- paste0(h_base, hues)

herb_df <- subset(fish_df, foodweb == "Herbivore")
herb_df <- herb_df %>%
  group_by(date, month, Dome, Treatment, outplant_time) %>%
  summarise(mean_biomass = sum(biomass)/(length(unique(Dome))),
            mean_abundance = sum(count)/length(unique(Dome)),
            mean_richness = n_distinct(fish_spp)/length(unique(Dome)))

p1 <- ggplot(herb_df, aes(outplant_time, mean_biomass/1000, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") +
  geom_point(size=4) +
  xlab("") +
  ylab("Mean biomass (kg)") +
  ggtitle("Herbivores") +
  scale_fill_manual(values=h_colors) +
  my_theme +
  theme(legend.position = "none")
p1

p2 <- ggplot(herb_df, aes(outplant_time, mean_abundance, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") +
  geom_point(size=4) +
  #scale_fill_fish_d("Clepticus_parrae", begin = 0.2, end =1) +
  xlab("") +
  ylab("Mean abundance") +
  ggtitle("Herbivores") +
  scale_fill_manual(values=h_colors) +
  my_theme +
  theme(legend.position = "none")
p2

kruskal.test(mean_biomass ~ outplant_time, data = herb_df)
kruskal.test(mean_abundance ~ outplant_time, data = herb_df)

# parrotfish and initial phase parrots


p_base <- "#0646CF"
p_colors <- paste0(p_base, hues)

scar_df <- subset(fish_df, latin_family == "Scaridae")

scar_df <- scar_df %>%
  group_by(date, month, Dome, Treatment, outplant_time) %>%
  summarise(mean_biomass = sum(biomass)/(length(unique(Dome))),
            mean_abundance = sum(count)/length(unique(Dome)),
            mean_richness = n_distinct(fish_spp)/length(unique(Dome)))

p3 <- ggplot(scar_df, aes(outplant_time, mean_biomass/1000, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") +
  geom_point(size=4) +
  xlab("") +
  ylab("Mean biomass (kg)") +
  ggtitle("Parrotfish") +
  scale_fill_manual(values=p_colors) +
  my_theme +
  theme(legend.position = "none")
p3

p4 <- ggplot(scar_df, aes(outplant_time, mean_abundance, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") +
  geom_point(size=4) +
  xlab("") +
  ylab("Mean abundance") +
  ggtitle("Parrotfish") +
  scale_fill_manual(values=p_colors) +
  my_theme +
  theme(legend.position = "none")
p4

kruskal.test(mean_biomass ~ outplant_time, data = scar_df)
kruskal.test(mean_abundance ~ outplant_time, data = scar_df)

# initial phase

i_base <- "#B35ABA"
i_colors <- paste0(i_base, hues)

initial_df <- fish_df[grepl("initial$", fish_df$common_name), ]

initial_df <- initial_df %>%
  group_by(date, month, Dome, Treatment, outplant_time) %>%
  summarise(mean_biomass = sum(biomass)/(length(unique(Dome))),
            mean_abundance = sum(count)/length(unique(Dome)),
            mean_richness = n_distinct(fish_spp)/length(unique(Dome)))

p5 <- ggplot(initial_df, aes(outplant_time, mean_biomass/1000, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") +
  geom_point(size=4) +
  #coord_cartesian(ylim =  c(0, 3))+ 
  xlab("") +
  ylab("Mean biomass (kg)") +
  ggtitle("Initial Parrotfish") +
  scale_fill_manual(values=i_colors) +
  my_theme +
  theme(legend.position = "none")
p5

p6 <- ggplot(initial_df, aes(outplant_time, mean_abundance, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") +
  geom_point(size=4) +
  xlab("") +
  ylab("Mean abundance") +
  ggtitle("Initial Parrotfish") +
  scale_fill_manual(values=i_colors) +
  my_theme +
  theme(legend.position = "none")
p6

kruskal.test(mean_biomass ~ outplant_time, data = initial_df)
kruskal.test(mean_abundance ~ outplant_time, data = initial_df)

# check if any mental damsel fish just took over the domes

d_base <- "#E28928"
d_colors <- paste0(d_base, hues)

damsel_df <- subset(fish_df, latin_family == "Pomacentridae")
damsel_df <- damsel_df %>%
  group_by(date, month, Dome, Treatment, outplant_time) %>%
  summarise(mean_biomass = sum(biomass)/(length(unique(Dome))),
            mean_abundance = sum(count)/length(unique(Dome)),
            mean_richness = n_distinct(fish_spp)/length(unique(Dome)))

p7 <- ggplot(damsel_df, aes(outplant_time, mean_biomass/1000, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") +
  geom_point(size=4) +
  xlab("Outplant time") +
  ylab("Mean biomass (kg)") +
  ggtitle("Damselfish") +
  scale_fill_manual(values=d_colors) +
  my_theme +
  theme(legend.position = "none")
p7

p8 <- ggplot(damsel_df, aes(outplant_time, mean_abundance, fill = as.factor(outplant_time))) +
  geom_boxplot(color = "black") +
  geom_point(size=4) +
  xlab("Outplant time") +
  ylab("Mean abundance") +
  ggtitle("Damselfish") +
  scale_fill_manual(values=d_colors) +
  my_theme +
  theme(legend.position = "none")
p8

kruskal.test(mean_biomass ~ outplant_time, data = damsel_df)
kruskal.test(mean_abundance ~ outplant_time, data = damsel_df)


png(file=file.path(output_wd, "fishgroup_boxplots.png"), height = 5000, width = 4000, res = 350)
(p1 + p2)/(p3 + p4)/(p5 + p6)/(p7 + p8) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 30)) 
dev.off()


##########################################################################################
################################# community composition ##################################

rm(d0,d5,d85,ks_result,m0,m5,m85,p1,p2,p3,size_df)

# communtiy matrix 

# for nMDS look at control community, day 0, day 5, day 85

fish_df <- fish_df %>%
  mutate(ord_group = ifelse(Treatment == "Control", "Control",
                            ifelse(outplant_time == "Before" & Treatment == "Outplants", "Before",
                                   ifelse(outplant_time == "Day 5" & Treatment == "Outplants", "Outplant Day 5",
                                          ifelse(outplant_time == "Day 85" & Treatment == "Outplants", "Outplant Day 85", NA)))))

# 2. Data Transformation: Create a count matrix for fish species
df2 <- fish_df %>%
  group_by(date, month, Dome, ord_group, fish_spp) %>%
  summarise(total_abundance = sum(count))

df2 <- df2 %>%
  pivot_wider(names_from = fish_spp, values_from = total_abundance)

df2[is.na(df2)] <- 0

mat1 <- as.matrix(df2[,-c(1:5)])
mat1 <- sqrt(mat1)
dist1 <- vegdist(mat1, method = "bray")

# permnova

set.seed(36)
comp <- adonis2(dist1~as.factor(df2$ord_group), data = df2, permutations = 9999)
comp
summary(comp)

# nmds 

set.seed(36)
m1 <- metaMDS(mat1, distance = "bray", k=2, autotransform = T, trymax = 75)
m1 # stress = 0.187

stressplot(m1)

# extract scores for figure 

scores <- data.frame(m1$points)

scores$ord_group <- df2$ord_group

# plot


hull_0 <- scores[scores$ord_group == "Outplant Day 0",][chull(scores[scores$ord_group == "Outplant Day 0", c("MDS1", "MDS2")]),]
hull_5 <- scores[scores$ord_group == "Outplant Day 5",][chull(scores[scores$ord_group == "Outplant Day 5", c("MDS1", "MDS2")]),]
hull_85 <- scores[scores$ord_group == "Outplant Day 85",][chull(scores[scores$ord_group == "Outplant Day 85", c("MDS1", "MDS2")]),]

hull_df <- rbind(hull_0, hull_5, hull_85)

fishualize("Clepticus_parrae", n=3)
fishualize::fish(option="Clepticus_parrae", n=3)

ord_1 <- #ggplot()
  ggplot(data=scores,aes(x=MDS1, y=MDS2, colour=ord_group, fill = ord_group)) +
  stat_ellipse(type='t',size =1.2, linetype = 1)+
  geom_point(position=position_jitter(.1), shape=19, size = 1)+
  my_theme +
  scale_color_manual(values=c("#E28928FF", "#7C43A9FF", "#0646CFFF"), labels =c("Before", "Day 5", "Day 85")) +
  scale_fill_manual(values=c("#E28928FF", "#7C43A9FF", "#0646CFFF")) +
  labs(color = "Outplant time") +
  guides(fill="none") +
  xlab("MDS1") + ylab("MDS2") +
  annotate("text", x = -.7, y = 1, label = "Stress = 0.187", color = "black", size = 4.5, fontface = "bold")
ord_1

png(file=file.path(output_wd, "ordinations_before_5_85.png"), height = 2000, width = 3000, res = 350)
ord_1
dev.off()

##########################################################################################
############################ dominant species before v after #############################

rm(comp, df, df2, df3, m2, mat1, mat2, ord_2, scores2, dist1, dist2)

abundance_df <- fish_df %>%
  group_by(fish_spp, outplant_time) %>%
  summarise(abundance =  sum(count))


# Assuming your original dataframe is named 'abundance_df'
top_species_df <- abundance_df %>%
  group_by(fish_spp, outplant_time) %>%
  summarize(total_abundance = sum(abundance)) %>%
  arrange(desc(total_abundance)) %>%
  head(43)


# Assuming your dataframe is named 'top_species_df'
# Convert 'outplant_time' to a factor for correct ordering on the heatmap
top_species_df$outplant_time <- factor(top_species_df$outplant_time, levels = c("Before", "Day 5", "Day 85"))

hist(log1p(top_species_df$total_abundance))
summary(log1p(top_species_df$total_abundance))

top_species_df$fish_spp <- gsub("_", " ", top_species_df$fish_spp)


# Determine the breaks for binning
breaks <- c(0, 50, 100, 500, 1000, Inf)

# Create bins based on the breaks
top_species_df$abundance_bin <- cut(top_species_df$total_abundance, breaks = breaks, include.lowest = TRUE, labels = FALSE)

# Convert abundance_bin to a factor for better presentation
top_species_df$abundance_bin <- as.factor(top_species_df$abundance_bin)

# Display the bins
levels(top_species_df$abundance_bin) <- paste("Bin", levels(top_species_df$abundance_bin))

top_species_df$abundance_bin <- factor(top_species_df$abundance_bin, levels = rev(levels(top_species_df$abundance_bin)))

fish(option = "Clepticus_parrae", n =5, direction = -1)

# Create the heatmap
heatmap_plot <- ggplot(top_species_df, aes(x = reorder(fish_spp, total_abundance), y = outplant_time, fill = (abundance_bin))) +
  geom_tile(color = "black") +
  labs(x = "Fish Species", y = "Outplant Time", fill = "Abundance") +
  #scale_fill_fish_d(option = "Clepticus_parrae", direction =-1, labels = c(1168,507,125,52,15)) +
  scale_fill_manual(values = c("#001847FF", "#0646CFFF", "#7C43A9FF", "#C464AAFF", "#E28928FF"), 
                    labels = c(1168,507,125,52,15)) +
  coord_flip() +
  my_theme +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, vjust = 1.5, face = "plain", size = 14), axis.text.y = element_text(face = "italic"))
heatmap_plot



png(file=file.path(output_wd, "species_heatmap.png"), height = 2000, width = 4000, res = 400)
heatmap_plot
dev.off()
