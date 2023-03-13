#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script tests fine fuels data for significance using the Wilcoxon rank sum tests

#Author: Jim Cronan
#Organization: US Forest Service
#Address:
#400 N 34th Street
#Suite 201
#Seattle, WA 98103
#Date Created: 2-Mar-2023

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
dt_csv <- read.csv("Derivative_table_fine_fuels.csv", header=TRUE, 
                   sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Common name for table showing pixels per category
dtp <- dt_csv

#Aggregate fine fuel loading categories into three used in analysis
# =< 2 tons/acre - typically longleaf pine with mean fire return interval 1-3 years
# 2.1-4.0 tons/acre - typically longleaf pine with mean fire return interval 4-10 years
# =< 4.0 tons/acre - typically longleaf pine with mean fire return interval greater than 10 years
dtp_low <- dtp$X1 + dtp$X2
dtp_med <- dtp$X3 + dtp$X4
dtp_high <- dtp$X5 + dtp$X6 + dtp$X7 + dtp$X8 + dtp$X9 + dtp$X10 + dtp$X11 + dtp$X12 + dtp$X13 + dtp$X14 + dtp$X15 + dtp$X16 + dtp$X17 + dtp$X18 + dtp$X19 + dtp$X20 + dtp$X21

dt <- data.frame(dtp[,1:3], low = dtp_low, med = dtp_med, hi = dtp_high)

#Convert rx fire scenarios to hectares (from acres)
dt$rx_fire[dtp$rx_fire == 50] <- 20
dt$rx_fire[dtp$rx_fire == 75] <- 30
dt$rx_fire[dtp$rx_fire == 100] <- 40
dt$rx_fire[dtp$rx_fire == 125] <- 50

#Is data normally distributed
hist(c(dt$low, dt$med, dt$hi))
#Definitely not
#Data also originates from the same sample and is tracked through time
#Both qualities of this dataset make it suitable for a Wilcoxon test
#https://www.sciencedirect.com/topics/medicine-and-dentistry/wilcoxon-signed-ranks-test#:~:text=Wilcoxon%20rank%2Dsum%20test%20is,their%20population%20mean%20ranks%20differ.

#Subset data at 10-yr intervals
dt2 <- dt[dt$rx_fire %in% c(20,30,40,50),]
dt10 <- dt2[dt2$sim_yr %in% c(10,20,30,40,50),]

#Create an id number for each row.
row_id <- dt10$run_no

#Two-way ANOVA analysis code from:
#https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/

#Create dataset for low fine fuel loading
ff_low <- data.frame(id = as.factor(row_id),
                     rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                     time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                     area = dt10$low)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_low %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_low, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_low %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, all groups have outliers, there is one group with extreme outliers: t50/75

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_low %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#Except for two combinations all are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_low, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
low.aov <- anova_test(
  data = ff_low, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(low.aov)

# Effect of treatment at each time point
one.way <- ff_low %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_low %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
print(pwc, n = 30)

# Effect of treatment at each time point
one.way <- ff_low %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for medium fine fuel loading
ff_med <- data.frame(id = as.factor(row_id),
                     rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                     time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                     area = dt10$med)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_med %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_med, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_med %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, all groups have outliers, there are no groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_med %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#Except for three combinations all are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_med, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
med.aov <- anova_test(
  data = ff_med, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(med.aov)

# Effect of treatment at each time point
one.way <- ff_med %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_med %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
print(pwc, n = 30)

# Effect of treatment at each time point
one.way <- ff_med %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for hiium fine fuel loading
ff_hi <- data.frame(id = as.factor(row_id),
                    rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                    time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                    area = dt10$hi)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_hi %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_hi, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_hi %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, all groups have outliers, there are no groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_hi %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#All are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_hi, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
hi.aov <- anova_test(
  data = ff_hi, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(hi.aov)

# Effect of treatment at each time point
one.way <- ff_hi %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_hi %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
print(pwc, n = 30)

# Effect of treatment at each time point
one.way <- ff_hi %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way
