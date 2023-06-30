## NLA ES inequality, WQ figures; see other files for GIS source code
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
               "GGally", "car","randomForest",
               "ggpubr", "grid")) # to ggarrange

#########################################
# # National Lakes Assessment site info preliminary EDA
# dt <- read.csv("nla_2017_site_information-data.csv") 
# 
# dt <- mutate_if(dt, is.character, as.factor) # 5721 obs, 97 sites sampled twice
# str(dt) # 5624
# # write.csv(unique(dt[,c(7,37,38)]), "xxx.csv") # confirm lat/longs are same
# # 5624 unique lakes
# summary(dt)

# NB: EVAL_CAT has denied and not_lake, factor levels below
# [1] Not_Lake             Lake_Shallow         Target_Inaccess      Lake_LT_4ha          Lake_Vegetated      
# [6] Lake_Saline          Lake_Special_Purpose Target_Denied        Target_Other         Lake_LT_1ha         
# [11] Unknown              Target_Sampled 

# dt$diam <- dt$PERIM_KM/pi # testing filtering by diameter, not recommended
# summary(dt$diam)
# hist(log10(dt$diam))

# plot(log10(dt$PERIM_KM) ~ log10(dt$AREA_HA))
# top_n(dt, 20, AREA_HA)
# summary(dt$AREA_HA)

# dt2 <- subset(dt, AREA_HA < 200) # <200 ha 5%; <50 ha 15% obs
# str(dt2) # why is the number of unique sites still the same??? 73 sites sampled twice, so should be 5345

# dt2 <- subset(dt2, VISIT_NO != 2) # NEVER do this again
# x <- subset(dt2, dt2$VISIT_NO == 2) # 73 sites sampled twice

# hist(dt2$PERIM_KM) # 86% lakes PERIM_KM < 5K


#########################################
# Exploring EJ (see Sergey's code for extractions)
# ej <- read.csv("results.csv") # previous version had a loop error
# setnames(ej, old = "ncca_site_id", new = "UNIQUE_ID" )
# ej2 <- unique(ej)

# pdf("nlaEj.pdf", width = 6.5, height = 4)
# ggplot(ej2, aes(x = low_inc_buf*100, y = low_minor_buf*100)) + #
#   geom_density_2d_filled() + #
#   geom_point(alpha = 0.5, size = 0.1, col = "white") + # can add white points if desired
#   geom_smooth(method = "loess", se = T, size = 0.5, alpha = 0.4, col = "white") + #, col = "black"
#   xlab("Low income, %") + ylab("Minority, %") +
#   ylim(0,100) +
#   theme_bw() + theme(panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), legend.position = "NONE")
# dev.off()

#########################################
# Indicator status
dt <- read.csv("nla2017_all_sites-visits_data_for_population_estimates_220705_0.csv") # downloaded 11/16/2022
dt <- dt[,c(2,9,15,16,31:50,66)]

ej <- read.csv("results.csv") # previous version had a loop error
setnames(ej, old = "ncca_site_id", new = "UNIQUE_ID" ) # this is NLA id not NCCA!
ej2 <- unique(ej) # remove resampled sites

dt2 <- join(dt, ej2, by = "UNIQUE_ID")

dt2 <- dt2[,-c(3:4,9:16,18:22,26,29,30)]

dt2 <- dt2[,-c(7,8,10)]
dt2 <- mutate_if(dt2, is.character, as.factor)
# summary(dt2)

# p1 + p2 NLA_SCI_STATUS
dt3 <- subset(dt2, NLA_SCI_STATUS != "Not Assessed")
dt3 <- dt3[complete.cases(dt3[,"NLA_SCI_STATUS"]),]
dt3 <- unique(dt3[,c(1,7:9)])
dt3 <- droplevels(dt3)
dt3$NLA_SCI_STATUS <- ifelse(dt3$NLA_SCI_STATUS == "Most Disturbed", "Poor",
                             ifelse(dt3$NLA_SCI_STATUS == "Moderately Disturbed", "Fair",
                                    "Good"))
p1 <- ggplot(dt3, aes(y = factor(NLA_SCI_STATUS, levels = c("Good", "Fair", "Poor")), x = low_minor_buf*100, 
                      col = factor(NLA_SCI_STATUS, levels = c("Good", "Fair", "Poor")))) + 
  geom_boxplot()+
  # facet_wrap(~ dt3$EPA_REG) +
  ylab("Sediment index") +
  theme_bw() + 
  scale_color_manual(values = c("grey", "orange", "red")) + 
  theme(axis.title = element_text(size = 13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "NONE")

p2 <- ggplot(dt3, aes(y = factor(NLA_SCI_STATUS, levels = c("Good", "Fair", "Poor")), x = low_inc_buf*100, 
                      col = factor(NLA_SCI_STATUS, levels = c("Good", "Fair", "Poor")))) +  
  geom_boxplot()+
  # facet_wrap(~ dt3$EPA_REG) +
  ylab("") +
  theme_bw() + 
  scale_color_manual(values = c("grey", "orange", "red")) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x=element_blank(),
        legend.position = "NONE")


# p3 + p4 chl a
dt3 <- subset(dt2, CHLA_COND != "Not Assessed") # none just keeping the flow constant here
dt3 <- dt3[complete.cases(dt3[,"CHLA_COND"]),] # none ditto
dt3 <- unique(dt3[,c(1,5,8,9)])
# dt3 <- droplevels(dt3)
p3 <- ggplot(dt3, aes(y = factor(CHLA_COND, levels = c("Good", "Fair", "Poor")), x = low_minor_buf*100,  
                      col = factor(CHLA_COND, levels = c("Good", "Fair", "Poor")))) + 
  geom_boxplot()+
  # facet_wrap(~ dt3$EPA_REG) +
  xlab("") + ylab("Chlorophyll a status") +
  theme_bw() + 
  scale_color_manual(values = c("grey", "orange", "red")) + 
  theme(axis.title = element_text(size = 13),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "NONE")

p4 <- ggplot(dt3, aes(y = factor(CHLA_COND, levels = c("Good", "Fair", "Poor")), x = low_inc_buf*100,  
                      col = factor(CHLA_COND, levels = c("Good", "Fair", "Poor")))) + 
  geom_boxplot()+
  # facet_wrap(~ dt3$EPA_REG) +
  ylab("") +
  theme_bw() + 
  scale_color_manual(values = c("grey", "orange", "red")) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "NONE")


# p5 + p6 Nitrogen
dt3 <- subset(dt2, NTL_COND != "Not Assessed") # none just keeping the flow constant here
dt3 <- dt3[complete.cases(dt3[,"NTL_COND"]),] # none ditto
dt3 <- unique(dt3[,c(1,4,8,9)])
# dt3 <- droplevels(dt3)
p5 <- ggplot(dt3, aes(y = factor(NTL_COND, levels = c("Good", "Fair", "Poor")), x = low_minor_buf*100,  
                      col = factor(NTL_COND, levels = c("Good", "Fair", "Poor")))) + 
  geom_boxplot()+
  ylab("Nitrogen status") +
  theme_bw() + 
  scale_color_manual(values = c("grey", "orange", "red")) + 
  theme(axis.title = element_text(size = 13), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "NONE")

p6 <- ggplot(dt3, aes(y = factor(NTL_COND, levels = c("Good", "Fair", "Poor")), x = low_inc_buf*100,  
                      col = factor(NTL_COND, levels = c("Good", "Fair", "Poor")))) + 
  geom_boxplot()+
  ylab("") +
  theme_bw() + 
  scale_color_manual(values = c("grey", "orange", "red")) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "NONE")

# p7 + p8 TP
dt3 <- subset(dt2, PTL_COND != "Not Assessed") # none just keeping the flow constant here
dt3 <- dt3[complete.cases(dt3[,"PTL_COND"]),] # none ditto
dt3 <- unique(dt3[,c(1,3,8,9)])
# dt3 <- droplevels(dt3)
p7 <- ggplot(dt3, aes(y = factor(PTL_COND, levels = c("Good", "Fair", "Poor")), x = low_minor_buf*100,  
                      col = factor(PTL_COND, levels = c("Good", "Fair", "Poor")))) + 
  geom_boxplot()+
  xlab("Minority, %") + ylab("Total phosphorus status") +
  theme_bw() + 
  scale_color_manual(values = c("grey", "orange", "red")) + 
  theme(axis.title = element_text(size = 13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "NONE")

p8 <- ggplot(dt3, aes(y = factor(PTL_COND, levels = c("Good", "Fair", "Poor")), x = low_inc_buf*100,  
                      col = factor(PTL_COND, levels = c("Good", "Fair", "Poor")))) + 
  geom_boxplot()+
  xlab("Low income, %") + ylab("") +
  theme_bw() + 
  scale_color_manual(values = c("grey", "orange", "red")) + 
  theme(axis.title = element_text(size = 13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position = "NONE")

# # p9 + p10 Trophic index
# dt3 <- subset(dt2, TROPHIC_STATE != "Not Assessed") # none just keeping the flow constant here
# dt3 <- dt3[complete.cases(dt3[,"TROPHIC_STATE"]),] # none ditto
# dt3 <- unique(dt3[,c(1,6,8,9)])
# # dt3 <- droplevels(dt3)
# p9 <- ggplot(dt3, aes(y = factor(TROPHIC_STATE, levels = c("Oligotrophic", "Mesotrophic",
#                                                            "Eutrophic", "Hypereutrophic")), x = low_minor_buf*100,  
#                       col = factor(TROPHIC_STATE, levels = c("Oligotrophic", "Mesotrophic",
#                                                         "Eutrophic", "Hypereutrophic")))) + 
#   geom_boxplot()+
#   xlab("Minority, %") + ylab("Trophic state") +
#   theme_bw() + 
#   scale_color_manual(values = c("green","grey", "orange", "red")) + 
#   theme(axis.text.x = element_text(size = 11), axis.title = element_text(size = 11),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), legend.position = "NONE")

pdf("nlaAllLargeFont.pdf", width = 8, height = 8)
# figure2 <- ggarrange(p1,p2,p3,p4,p5,p6, nrow = 3, ncol = 2)
figure2 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 4, ncol = 2)
# txt <- grobTree(textGrob("A", x = 0.05, y = 0.99, hjust=0, gp = gpar(fontsize = 12)))
# txt2 <- grobTree(textGrob("B", x = 0.05, y = 0.75, hjust=0, gp = gpar(fontsize = 12)))
# txt3 <- grobTree(textGrob("C", x = 0.05, y = 0.50, hjust=0, gp = gpar(fontsize = 12)))
# txt4 <- grobTree(textGrob("D", x = 0.05, y = 0.26, hjust=0, gp = gpar(fontsize = 12)))
figure2
# + annotation_custom(txt) + annotation_custom(txt2) + annotation_custom(txt3) + annotation_custom(txt4)
dev.off()



fit <- aov(low_minor_buf ~ NTL_COND, data = dt3)
summary(fit)
TukeyHSD(fit)


# dt3$TROPHIC_STATE <- ifelse(dt3$TROPHIC_STATE == "Oligotrophic", 1,
#                             ifelse(dt3$TROPHIC_STATE == "Mesotrophic", 2,
#                                    ifelse(dt3$TROPHIC_STATE == "Eutrophic", 4, 5)))
# ggplot(dt2, aes(y = as.numeric(TROPHIC_STATE), x = low_minor_buf*100)) + 
#   geom_point(alpha = 0.7, size = 1.5) +
#   geom_smooth(method = "lm", se = F, size = 0.8, alpha = 0.6) +
#   xlab("Low income, %") + ylab("") +
#   # facet_wrap(~ dt2$EPA_REG) +
#   theme_bw() + 
#   scale_color_viridis(direction = 1, discrete = T) +
#   theme(axis.text.x = element_text(size = 11), axis.title = element_text(size = 11),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), legend.position = "NONE")
# 

#########################################
# Combined indices
dt <- read.csv("nla2017_all_sites-visits_data_for_population_estimates_220705_0.csv") # downloaded 11/16/2022
dt <- dt[,c(2,9,15,16,29:50)]

ej <- read.csv("results.csv") # previous version had a loop error
setnames(ej, old = "ncca_site_id", new = "UNIQUE_ID" ) # this is NLA id not NCCA!
ej2 <- unique(ej) # remove resampled sites

dt2 <- join(dt, ej2, by = "UNIQUE_ID")

dt2 <- dt2[,-c(3:4,6,11:18,21)]

dt2$NLA_SCI_STATUS <- ifelse(dt2$NLA_SCI_STATUS == "Most Disturbed", "Poor",
                             ifelse(dt2$NLA_SCI_STATUS == "Moderately Disturbed", "Fair",
                                    ifelse(is.na(dt2$NLA_SCI_STATUS), NA, "Good")))
dt2[dt2 == "Good"] <- 3
dt2[dt2 == "Fair"] <- 2
dt2[dt2 == "Poor"] <- 1

dt2$TROPHIC_STATE <- ifelse(dt2$TROPHIC_STATE == "Eutrophic", 1,
                         ifelse(dt2$TROPHIC_STATE == "Mesotrophic", 2,
                                ifelse(dt2$TROPHIC_STATE == "Oligotrophic",3,0)))

dt2$overall <- as.numeric(dt2$ACID_COND) + as.numeric(dt2$PTL_COND) + as.numeric(dt2$NTL_COND) + 
  as.numeric(dt2$CHLA_COND) + 2*as.numeric(dt2$TROPHIC_STATE) + as.numeric(dt2$ZOOP_MMI_COND_2017) 
  # as.numeric(dt2$LITRIPCVR_COND) + as.numeric(dt2$LITCVR_COND) + as.numeric(dt2$RVEG_COND) +
  # as.numeric(dt2$RDIS_COND) + as.numeric(dt2$NLA_SCI_STATUS)

pdf("allNla.pdf", width = 6.5, height = 4)
ggplot(dt2, aes(x = low_minor_buf*100, y = overall)) + #
  geom_density_2d_filled() + #
  geom_point(alpha = 0.5, size = 0.1, col = "white") + # can add white points if desired
  geom_smooth(method = "lm", se = T, size = 0.5, alpha = 0.4, col = "white") + #, col = "black"
  xlab("Minority, %") + ylab("Overall habitat quality") +
  # ylim(0,100) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = "NONE")
dev.off()

### With E.coli
dt2 <- join(dt, ej2, by = "UNIQUE_ID")
dt2 <- dt2[,-c(3:4,6,12:14,16,18,21)]

dt2$NLA_SCI_STATUS <- ifelse(dt2$NLA_SCI_STATUS == "Most Disturbed", "Poor",
                             ifelse(dt2$NLA_SCI_STATUS == "Moderately Disturbed", "Fair",
                                    ifelse(is.na(dt2$NLA_SCI_STATUS), NA, "Good")))
dt2[dt2 == "Above Benchmark"] <- -2
dt2[dt2 == "At or Below Benchmark"] <- 0
dt2[dt2 == "Good"] <- 3
dt2[dt2 == "Fair"] <- 2
dt2[dt2 == "Poor"] <- 1

dt2$TROPHIC_STATE <- ifelse(dt2$TROPHIC_STATE == "Eutrophic", 1,
                            ifelse(dt2$TROPHIC_STATE == "Mesotrophic", 2,
                                   ifelse(dt2$TROPHIC_STATE == "Oligotrophic",3,0)))

dt2$overall <- as.numeric(dt2$ACID_COND) + as.numeric(dt2$PTL_COND) + as.numeric(dt2$NTL_COND) + 
  as.numeric(dt2$CHLA_COND) + as.numeric(dt2$TROPHIC_STATE) + as.numeric(dt2$ZOOP_MMI_COND_2017) +
  as.numeric(dt2$ATZ_EPA_COND_3.4) + as.numeric(dt2$MICX_EPA_COND) + as.numeric(dt2$ECOLI_EPA_COND) +
  as.numeric(dt2$LITRIPCVR_COND) + as.numeric(dt2$LITCVR_COND) + as.numeric(dt2$RVEG_COND) +
  as.numeric(dt2$RDIS_COND) + as.numeric(dt2$NLA_SCI_STATUS)

pdf("allNlaInc.pdf", width = 5, height = 4)
ggplot(dt2, aes(x = low_inc_buf*100, y = overall)) + #
  geom_density_2d_filled() + #
  geom_point(alpha = 0.5, size = 0.1, col = "white") + # can add white points if desired
  geom_smooth(method = "lm", se = T, size = 0.5, alpha = 0.4, col = "white") + #, col = "black"
  xlab("Minority, %") + ylab("Overall habitat quality index") +
  # ylim(0,100) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.position = "NONE")
dev.off()

fit <- lm(low_minor_buf ~ overall, dt2)
summary(fit)

#########################################
# RF
dt <- read.csv("nla2017_all_sites-visits_data_for_population_estimates_220705_0.csv") # downloaded 11/16/2022
dt <- dt[,c(2,9,15,16,29:50)]

ejAll <- read.csv("resultsRf.csv") # from MSI
subs <- read.csv("predictorsMachineLearning.csv")
# subs <- subset(subs, use != 0)
subs <- subset(subs, use == 2)
select.me <- subs$GDB.Fieldname

ejAll2 <- cbind(UNIQUE_ID = ejAll$UNIQUE_ID, ejAll[ , names(ejAll) %in% select.me])
ejAll2 <- unique(ejAll2[,-2]) # -3; remove resampled sites

dt2 <- join(dt, ejAll2, by = "UNIQUE_ID")
dt2 <- dt2[,-c(3:4,6,12:14,16,18,21)]

dt2$NLA_SCI_STATUS <- ifelse(dt2$NLA_SCI_STATUS == "Most Disturbed", "Poor",
                             ifelse(dt2$NLA_SCI_STATUS == "Moderately Disturbed", "Fair",
                                    ifelse(is.na(dt2$NLA_SCI_STATUS), NA, "Good")))
dt2[dt2 == "Above Benchmark"] <- -2
dt2[dt2 == "At or Below Benchmark"] <- 0
dt2[dt2 == "Good"] <- 3
dt2[dt2 == "Fair"] <- 2
dt2[dt2 == "Poor"] <- 1

dt2$TROPHIC_STATE <- ifelse(dt2$TROPHIC_STATE == "Eutrophic", 1,
                            ifelse(dt2$TROPHIC_STATE == "Mesotrophic", 0,
                                   ifelse(dt2$TROPHIC_STATE == "Oligotrophic",0,1)))

dt2$overall <- as.numeric(dt2$ACID_COND) + as.numeric(dt2$PTL_COND) + as.numeric(dt2$NTL_COND) + 
  as.numeric(dt2$CHLA_COND) + as.numeric(dt2$TROPHIC_STATE) + as.numeric(dt2$ZOOP_MMI_COND_2017) +
  as.numeric(dt2$ATZ_EPA_COND_3.4) + as.numeric(dt2$MICX_EPA_COND) + as.numeric(dt2$ECOLI_EPA_COND) +
  as.numeric(dt2$LITRIPCVR_COND) + as.numeric(dt2$LITCVR_COND) + as.numeric(dt2$RVEG_COND) +
  as.numeric(dt2$RDIS_COND) + as.numeric(dt2$NLA_SCI_STATUS)


fit <- randomForest(as.factor(dt2$NTL_COND) ~ ., dt2[,-c(1:17)], samp.size = c(546,546),
                    ntree = 10000, keep.forest = TRUE,
                    importance = TRUE, na.action = na.omit) # 

fit <- randomForest(overall ~ ., dt2[,-c(1:17,26:29)], ntree = 20000, keep.forest = TRUE,
                    importance = TRUE, na.action = na.omit) # 

fit # model summary to derive % variation explained
pdf("accrSF.pdf", width = 5.5, height = 4)
varImpPlot(fit, type = 1, main = "")
dev.off()

install_load(c("ggExtra", "ggcorrplot"))
corr <- cor(dt2[,c(18:30)], use = "complete.obs", method = "spearman") #

# pdf("crossCorrPlantPredictors.pdf", width = 6, height = 6)
pdf("crossCorrMar9.pdf", width = 10, height = 10)
ggcorrplot(corr, hc.order = F,
           type = "lower",
           lab = TRUE,
           lab_col = "white",
           lab_size = 2,
           # method="circle",
           insig = "blank", outline.col = "white",
           colors = c("dark green", "white", "dark blue"),
           ggtheme = theme_bw) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = c(0.2,0.75))
# dev.off()
