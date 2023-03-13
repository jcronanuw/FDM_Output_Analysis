#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script tests vegetation cover data for significance using
#a two-way repeated measures ANOVA with post-hoc comaparison tests

#Author: Jim Cronan
#Organization: US Forest Service
#Address:
#400 N 34th Street
#Suite 201
#Seattle, WA 98103
#Date Created: 7-Mar-2023

library(MASS)
library(rstatix)#anova_test()
library(ggplot2)
library(ggpubr)#ggboxplot()
#################################################################################################
#################################################################################################
#DATA INPUTS

#Which computer are you using?
#USFS
usfs <- "C:/Users/jcronan/OneDrive - USDA/"
pers <- "C:/Users/james/"

#################################################################################################
setwd(paste(pers, "Documents/FDM_2023_Simulation_Data/Step_05_Derivative_Tables", sep = ""))

#Import input parameters
dt_csv <- read.csv("Derivative_table_cover.csv", header=TRUE, 
                   sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Common name for table showing pixels per category
dtp <- dt_csv

#Aggregate mean fire interval into categories
#X1 - long needle pine
#X2 - mixed broadleaf pine
#X3 - broadleaf
#X4 - short-needle pine
#X5 - tall shrub
dtp_longleaf <- dtp$X1
dtp_mixed <- dtp$X2
dtp_broad <- dtp$X3
dtp_sand <- dtp$X4
dtp_shrub <- dtp$X5

dt <- data.frame(dtp[,1:3], longleaf = dtp_longleaf, mixed = dtp_mixed,
                 broad = dtp_broad, sand = dtp_sand, shrub = dtp_shrub)

#Convert rx fire scenarios to hectares (from acres)
dt$rx_fire[dtp$rx_fire == 50] <- 20
dt$rx_fire[dtp$rx_fire == 75] <- 30
dt$rx_fire[dtp$rx_fire == 100] <- 40
dt$rx_fire[dtp$rx_fire == 125] <- 50

#Is data normally distributed
hist(c(dtp_longleaf,
       dtp_mixed,
       dtp_broad,
       dtp_sand,
       dtp_shrub))
#Definitely not
#Data also originates from the same sample and is tracked through time
#Both qualities of this dataset make it suitable for a Wilcoxon test
#https://www.sciencedirect.com/topics/medicine-and-dentistry/wilcoxon-signed-ranks-test#:~:text=Wilcoxon%20rank%2Dsum%20test%20is,their%20population%20mean%20ranks%20differ.

#Subset data at 10-yr intervals
dt2 <- dt[dt$rx_fire %in% c(20,30,40,50),]
dt10 <- dt2[dt2$sim_yr %in% c(10,20,30,40,50),]

#Create an id number for each row.
row_id <- dt10$run_no

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for longleaf MFI (> 20 years)
ff_longleaf <- data.frame(id = as.factor(row_id),
                          rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                          time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                          area = dt10$longleaf)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_longleaf %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_longleaf, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_longleaf %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, nine groups have outliers, there are five groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_longleaf %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#Except for one, all are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_longleaf, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
longleaf.aov <- anova_test(
  data = ff_longleaf, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(longleaf.aov)

# Effect of treatment at each time point
one.way <- ff_longleaf %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_longleaf %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
print(pwc, n = 30)

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for mixed MFI (> 20 years)
ff_mixed <- data.frame(id = as.factor(row_id),
                       rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                       time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                       area = dt10$mixed)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_mixed %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_mixed, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_mixed %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, three groups have outliers, there are zero groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_mixed %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#All are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_mixed, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
mixed.aov <- anova_test(
  data = ff_mixed, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(mixed.aov)

# Effect of treatment at each time point
one.way <- ff_mixed %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_mixed %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
print(pwc, n = 30)

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for broad MFI (> 20 years)
ff_broad <- data.frame(id = as.factor(row_id),
                       rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                       time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                       area = dt10$broad)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_broad %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_broad, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_broad %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, nine groups have outliers, there are three groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_broad %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#Except for two, all are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_broad, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
broad.aov <- anova_test(
  data = ff_broad, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(broad.aov)

# Effect of treatment at each time point
one.way <- ff_broad %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_broad %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
print(pwc, n = 30)

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for sand MFI (> 20 years)
ff_sand <- data.frame(id = as.factor(row_id),
                      rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                      time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                      area = dt10$sand)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_sand %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_sand, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_sand %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, eight groups have outliers, there are zero groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_sand %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#Except for two, all are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_sand, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
sand.aov <- anova_test(
  data = ff_sand, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(sand.aov)

# Effect of treatment at each time point
one.way <- ff_sand %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_sand %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
print(pwc, n = 30)

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for shrub MFI (> 20 years)
ff_shrub <- data.frame(id = as.factor(row_id),
                       rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                       time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                       area = dt10$shrub)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_shrub %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_shrub, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_shrub %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, ten groups have outliers, there are two groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_shrub %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#All are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_shrub, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
shrub.aov <- anova_test(
  data = ff_shrub, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(shrub.aov)

# Effect of treatment at each time point
one.way <- ff_shrub %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_shrub %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
print(pwc, n = 30)

