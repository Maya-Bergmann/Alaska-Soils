# Learning R
# Alaska Soil Data
# Maya Bergmann
# 2/25/2021

# Loading packages----------------------

#basic packages
library(usethis)
library(devtools)
library(tidyverse)
library(plyr)

#ggplot color packages
install.packages("nord")
install.packages("PNWColors")
devtools::install_github("kaizadp/soilpalettes")
library("nord")
library("PNWColors")
library("soilpalettes")

#statistics packages
library(agricolae)

#ggplot set up--------------------------

theme_mb <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "right",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=0.25, fill = NA),
          
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}


# Loading data--------------------------
akdata_csv = read.csv("Processed/MASTER_DATA_FILE_Updated2.csv")


# ggplots--------------------------
akdata_csv %>% 
  filter(!is.na(pH)) %>%
  mutate(landform = recode(landform, "Ridgetop" = "ridgetop"),
         Parent.Material = recode(Parent.Material,
                                  "Volcanic graywacke? - Colluvium" =
                                    "Volcanic - Colluvium")) %>%
  ggplot() +
  geom_point(aes(x = pH, y = as.numeric(max_depth_cm), 
                 color = Parent.Material, shape = landform)) +
  theme_mb() +
  scale_y_reverse() +
  labs(title = "pH across Juneau Soils",
       y = "depth, cm",
       x = "pH") +
  facet_grid(landform~Mapped.Lithologic.unit) +
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 15)))



akdata_select %>% 
  filter(!is.na(pH) & !is.na(Lithology)) %>%
  ggplot() +
  geom_point(aes(x = pH, y = as.numeric(max_depth_cm), 
                 color = Lithology, shape = landform)) +
  theme_mb() +
  scale_y_reverse() +
  labs(title = "pH across Juneau Soils",
       y = "depth, cm",
       x = "pH") +
  facet_grid(landform~Lithology) +
  scale_color_manual(values = (PNWColors::pnw_palette("Starfish", 3)))



akdata_select %>% 
  filter(!is.na(pH) & !is.na(Origin)) %>%
  ggplot() +
  geom_point(aes(x = pH, y = as.numeric(max_depth_cm), 
                 color = Origin, shape = landform), size = 5, 
             alpha = 0.5) +
  theme_mb() +
  scale_y_reverse() +
  labs(title = "pH across Juneau Soils",
       y = "depth, cm",
       x = "pH") +
  facet_grid(landform~Origin) +
  scale_color_manual(values = (PNWColors::pnw_palette("Mushroom", 3)))



akdata_select %>% 
  filter(!is.na(TC_perc_2) & !is.na(Lithology)) %>%
  ggplot() +
  geom_point(aes(x = TC_perc_2, y = as.numeric(max_depth_cm), 
                 color = landform, shape = landform), size = 3) +
  theme_mb() +
  scale_y_reverse() +
  labs(title = "Total Carbon across Juneau Soils",
       y = "depth, cm",
       x = "Total Carbon, percent") +
  facet_grid(landform~Lithology) +
  scale_color_manual(values = (PNWColors::pnw_palette("Starfish", 3)))



akdata_select %>% 
  filter(!is.na(TC_perc_2) & !is.na(Origin)) %>%
  ggplot() +
  geom_point(aes(x = TC_perc_2, y = as.numeric(max_depth_cm), 
                 color = landform, shape = landform), size = 3) +
  theme_mb() +
  scale_y_reverse() +
  labs(title = "Total Carbon across Juneau Soils",
       y = "depth, cm",
       x = "Total Carbon, percent") +
  facet_grid(landform~Origin) +
  scale_color_manual(values = (PNWColors::pnw_palette("Sunset2", 3)))
# data processing--------------------------
akdata_select = akdata_csv %>% 
  mutate(landform = recode(landform, "Ridgetop" = "ridgetop")) %>% 
  mutate(Lithology = case_when(grepl("residuum", Parent.Material)~ "residuum",
                               grepl("colluvium", Parent.Material)~"colluvium",
                               grepl("till", Parent.Material)~"till"),
         Origin = case_when(grepl("Slate", Parent.Material)~"Slate",
                            grepl("Tonalite", Parent.Material)~"Tonalite",
                            grepl("Volcanic", Parent.Material)~'Volcanic'))

