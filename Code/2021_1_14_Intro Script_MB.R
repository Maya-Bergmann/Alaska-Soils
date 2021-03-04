# Learning R
# Introductory Script
# Maya Bergmann
# 1/14/2021

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

data_csv = read.csv("Processed/2021_1_14_connectivityvolumes_ER.csv")
ghg_csv = read.csv("Processed/ghg_depth.csv")

# Processing data-----------------------

# ggplots-----------------------
data_csv %>% 
  filter(conn_pore_perc>"0.05") %>% 
  ggplot() +
  geom_point(aes(x=conn_pore_perc, 
                 y=conn_water_perc, color= water, size=3, alpha=0.5)) +
  labs(y="Connected water, %", x="Connected pore, %") +
  theme_mb() + 
  scale_color_manual(values = pnw_palette("Bay", 2))

data2 = data_csv %>% 
  filter(conn_pore_perc>"0.05") %>% 
  ggplot() +
  geom_point(aes(x=conn_pore_perc, 
                 y=conn_water_perc, color= water, size=3, alpha=0.5)) +
  labs(y="Connected water, %", x="Connected pore, %") +
  theme_mb() + 
  scale_color_manual(values = pnw_palette("Bay", 2))
data2


# ggplots and processing: respiration-----------------------
ghg_csv %>% 
  mutate(day = factor(day, levels = c("day1", "day4", "day7", "day14"))) %>% 
  ggplot(aes(x = gain_ug_g_oc, y = mid, 
             color = day)) + 
  geom_line(orientation = "y") +
  geom_point() +
  scale_color_manual(values = (PNWColors::pnw_palette("Bay", 4))) + 
  ggtitle("respiration") +
  theme_mb() +
  facet_grid(site~trmt) +
  scale_y_reverse()


