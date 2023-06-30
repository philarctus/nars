## All histograms for NCCA/NLA analyses; see other files for GIS source code, WQ analyses
## started Aug 2022, updated Jan 2023
## author: sergey, katya
## R version 4.0.2 "Taking Off Again"

#install.packages('rstudioapi')

## Set working directory to the file location (only RStudio) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## This function takes a character vector of package names and attempts to load them, or install them if loading fails
install_load <- function(x){
  for( i in x ){
    ## require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      ##  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      ##  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

## Required packages 
install_load(c("rgdal", "sf", "ggplot2", "rgeos", "sp", "raster", "reshape2", "stringi", "scales", "plyr", "dplyr", 
               "readxl", "stringr", "janitor", "data.table", "purrr", "lubridate", "janitor", "vroom", "viridis",
               "GGally", "car", "latex2exp", "cowplot",
               "ggpubr", "grid")) # to ggarrange

#########################################
# All histograms
ej <- read.csv("results.csv") # previous version had a loop error
setnames(ej, old = "ncca_site_id", new = "UNIQUE_ID" )
ej2 <- unique(ej)
dt <- read.csv("nla2017_all_sites-visits_data_for_population_estimates_220705_0.csv") # downloaded 11/16/2022
dt <- dt[,c(2,9,15,16,31:50,66)]
dt2 <- join(dt, ej2, by = "UNIQUE_ID")

# p1 + p2 are NLA lake centroids
p1 <- ggplot(dt2, aes(x = low_minor_buf*100)) + # median = 0.07525
  geom_histogram(aes(fill=..count..)) + 
  # scale_fill_gradient(low = "red", high = "darkgreen") +
  scale_fill_viridis(direction = -1) +
  geom_vline(xintercept = 7.52, col = "red", linetype = "dashed", linewidth = 0.8) +
  xlab("") +
  ylab("Count NLA") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = "NONE")


p2 <- ggplot(dt2, aes(low_inc_buf*100)) + # 0.2883
  geom_histogram(aes(fill=..count..)) + #
  scale_fill_viridis(direction = -1) +
  geom_vline(xintercept = 28.82, col = "red", linetype = "dashed", linewidth = 0.8) +
  xlab("") +
  ylab("") +
  xlim(0,100) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = "NONE") 
  # theme(plot.margin=margin(l=-0.5,unit="cm"))

# p3 + p4 are NCCA sites
# ncca <- read.csv("C:/Users/ekovalen/Documents/Data management/ncca/es_inequality/ncca2010finalOct2022.csv")
ncca <- read.csv("ncca2010finalOct2022.csv")
p3 <- ggplot(ncca, aes(low_minor_buf*100)) + # 0.09760
  geom_histogram(aes(fill=..count..)) + #
  scale_fill_viridis(direction = -1) +
  geom_vline(xintercept = 9.76, col = "red", linetype = "dashed", linewidth = 0.8) +
  xlab("") +
  ylab("Count NCCA") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = "NONE")


p4 <- ggplot(ncca, aes(low_inc_buf*100)) + # 0.2871
  geom_histogram(aes(fill=..count..)) + #
  scale_fill_viridis(direction = -1) +
  geom_vline(xintercept = 28.71, col = "red", linetype = "dashed", linewidth = 0.8) +
  xlab("") +
  ylab("") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = "NONE") 
  # theme(plot.margin=margin(l=-0.5,unit="cm"))

# this is all census blocks 2020 220K obs
all <- read.csv("EJSCREEN_2020_StatePctile.csv")
p5 <- ggplot(all, aes(MINORPCT*100)) + # 0.27791
  geom_histogram(aes(fill=..count..)) + #
  scale_fill_viridis(direction = -1) +
  geom_vline(xintercept = 27.79, col = "red", linetype = "dashed", linewidth = 0.8) +
  xlab("Minority, %") + 
  ylab(TeX("Count all census $(\\it{\\times 10^{3}})$")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = "NONE") +
  # scale_y_continuous(labels = scales::label_number_si())
  ## scaling the y-axis to get rid of extra zeros
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3))

p6 <- ggplot(all, aes(LOWINCPCT*100)) + # 0.3053
  geom_histogram(aes(fill=..count..)) + #
  scale_fill_viridis(direction = -1) +
  geom_vline(xintercept = 30.53, col = "red", linetype = "dashed", linewidth = 0.8) +
  xlab("Low income, %") + 
  ylab("") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = "NONE") + 
  # theme(plot.margin=margin(l=-0.5,unit="cm")) +
  # scale_y_continuous(labels = scales::label_number_si())
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3))

pdf("histAllViridis.pdf", width = 6, height = 6)
# figure4 <- ggarrange(p1,p2,p3,p4,p5,p6, nrow = 3, ncol = 2)
# using cowplot::plot_grid to align multiple ggplot2 plots vertically
plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, align = "v")
# txt <- grobTree(textGrob("A", x = 0.05, y = 0.99, hjust=0, gp = gpar(fontsize = 12)))
# txt2 <- grobTree(textGrob("B", x = 0.05, y = 0.75, hjust=0, gp = gpar(fontsize = 12)))
# txt3 <- grobTree(textGrob("C", x = 0.05, y = 0.50, hjust=0, gp = gpar(fontsize = 12)))
# txt4 <- grobTree(textGrob("D", x = 0.05, y = 0.26, hjust=0, gp = gpar(fontsize = 12)))
# figure4 
# + annotation_custom(txt) + annotation_custom(txt2) + annotation_custom(txt3) + annotation_custom(txt4)
dev.off()


