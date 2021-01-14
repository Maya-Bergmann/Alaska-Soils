# Learning R
# Introductory Script
# Maya Bergmann
# 1/14/2021

# Loading packages----------------------

#basic packages
library(usethis)
library(devtools)
library(tidyverse)

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

theme_er <- function() {  # this for all the elements common across plots
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

data_csv = read.csv("processed/2021_1_14_connectivityvolumes_ER.csv")

# Processing data-----------------------
