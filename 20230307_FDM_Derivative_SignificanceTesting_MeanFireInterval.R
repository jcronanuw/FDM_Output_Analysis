#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script tests mean fire interval data for significance using
#a tway-way repated measures ANOVA with post-hoc comaparison tests

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
dt_csv <- read.csv("Derivative_table_mfri.csv", header=TRUE, 
                   sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Common name for table showing pixels per category
dtp <- dt_csv

#Aggregate mean fire interval into categories
#1-3 years: frequently burned longleaf pine
#4-8 years: infrequently burned longleaf pine
#9-20 years: unburned longleaf pine with hardwood understory
#> 20 years: likely bottomland hardwoods, oak, or sand pine.
dtp_high <- dtp$X1
dtp_moderate <- dtp$X2
dtp_low <- dtp$X3
dtp_unburned <- dtp$X4

dt <- data.frame(dtp[,1:3], high = dtp_high, moderate = dtp_moderate,
                 low = dtp_low, unburned = dtp_unburned)

#Convert rx fire scenarios to hectares (from acres)
dt$rx_fire[dtp$rx_fire == 50] <- 20
dt$rx_fire[dtp$rx_fire == 75] <- 30
dt$rx_fire[dtp$rx_fire == 100] <- 40
dt$rx_fire[dtp$rx_fire == 125] <- 50

#Is data normally distributed
hist(c(dtp_high,
       dtp_moderate,
       dtp_low,
       dtp_unburned))
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
#Create dataset for high-frequency(MFI 1-3 years) prescribed fire
ff_high <- data.frame(id = as.factor(row_id),
                      rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                      time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                      area = dt10$high)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_high %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_high, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_high %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, all groups have outliers, there are two groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_high %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#Except for three, sll are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_high, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
high.aov <- anova_test(
  data = ff_high, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(high.aov)

# Effect of treatment at each time point
one.way <- ff_high %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_high %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
print(pwc, n = 30)

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for moderate MFI (4-8 years)
ff_moderate <- data.frame(id = as.factor(row_id),
                          rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                          time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                          area = dt10$moderate)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_moderate %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_moderate, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_moderate %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, all groups have outliers, there are two groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_moderate %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#Except for three, sll are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_moderate, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
moderate.aov <- anova_test(
  data = ff_moderate, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(moderate.aov)

# Effect of treatment at each time point
one.way <- ff_moderate %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_moderate %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
print(pwc, n = 30)

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for low MFI (9-20 years)
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
#Yes, three groups have outliers, there are no groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_low %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#Except for two, sll are normal (P > 0.05)

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

##############################################################################################
##############################################################################################
##############################################################################################
#Create dataset for unburned MFI (> 20 years)
ff_unburned <- data.frame(id = as.factor(row_id),
                          rx_fire = as.factor(paste("rx", dt10$rx_fire, sep = "")),
                          time = as.factor(paste("t", dt10$sim_yr, sep = "")), 
                          area = dt10$unburned)

#Group the data by treatment and time, and then compute some summary statistics 
#of the score variable: mean and sd (standard deviation).
ff_unburned %>%
  group_by(rx_fire, time) %>%
  get_summary_stats(area, type = "mean_sd")

#Create box plots of the score colored by treatment groups:
options(scipen = 999)
bxp <- ggboxplot(
  ff_unburned, x = "time", y = "area",
  color = "rx_fire", palette = "jco")
bxp

#Outliers
set.seed(123)
ff_unburned %>%
  group_by(rx_fire, time) %>%
  identify_outliers(area)
#Yes, two groups have outliers, there are zero groups with extreme outliers.

#Compute Shapiro-Wilk test for each combinations of factor levels:
set.seed(123)
ff_unburned %>%
  group_by(rx_fire, time) %>%
  shapiro_test(area)
#Except for two, all are normal (P > 0.05)

#Create QQ plot for each cell of design:
ggqqplot(ff_unburned, "area", ggtheme = theme_bw()) +
  facet_grid(time ~ rx_fire, labeller = "label_both")
#From the plot above, as all the points fall approximately along the reference line, we can assume normality.

#ANOVA
unburned.aov <- anova_test(
  data = ff_unburned, dv = area, wid = id,
  within = c(rx_fire, time)
)
get_anova_table(unburned.aov)

# Effect of treatment at each time point
one.way <- ff_unburned %>%
  group_by(time) %>%
  anova_test(dv = area, wid = id, within = rx_fire) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between treatment groups
pwc <- ff_unburned %>%
  group_by(time) %>%
  pairwise_t_test(
    area ~ rx_fire, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
print(pwc, n = 30)

