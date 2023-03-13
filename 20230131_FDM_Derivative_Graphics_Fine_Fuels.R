#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script generates graphics from tabular data for fine fuels tables produced
#in 20230131_FDM_Derivative_Processing_Fine_Fuels.R scrip in this GitHub repository.
#GitHub Repository: FDM_Output_Analysis
#Graphics:
#1) Line graphs
#2) Box plots

#Author: Jim Cronan
#Organization: US Forest Service
#Address:
#400 N 34th Street
#Suite 201
#Seattle, WA 98103
#Date Created: 1-Feb-2023

library(fields)#for set.panel()
library(ggplot2)#for boxplots
library(stringr)#to wrap legend title
library(pander)
library(lubridate)
library(egg) ##ggarrange()
library(grid) #textGrob()
library(ggpubr) #annotate_figure()
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

#Look at totals for each row. This is the number of pixels in each map. These numbers should
#all be the same, or at least in a very narrow range.
dt_sum <- apply(dt_csv[,4:24],1,sum)
range(dt_sum)
plot(dt_sum)
total_pixels <- mean(dt_sum)
#They are all exactly the same

#Map resolution (acres per 30x30 meter pixel)
MapRes <- 0.222395
MapRes * total_pixels
#Acreage of EAFB is 432,883.6 (excludes fuelbeds 5099000 (developed) and 6000000 (water))
#Total reported acreage of Eglin Air Force Base is 464,000 acres
#Total area from these maps is:
#2,100,256 pixels (0.222395 acres per pixel)
#467,086 acres
#432,878 acres (1,946,463 pixels) are vegetated terrestrial.

total_area <- 2100256

#Common name for table showing pixels per category
dtp <- dt_csv

dtp_0_2 <- dtp$X1 + dtp$X2
dtp_2_4 <- dtp$X3 + dtp$X4
dtp_4_6 <- dtp$X5 + dtp$X6
dtp_6_10 <- dtp$X7 + dtp$X8 + dtp$X9 + dtp$X10
dtp_11_ <- dtp$X11 + dtp$X12 + dtp$X13 + dtp$X14 + dtp$X15 + dtp$X16 + dtp$X17 + dtp$X18 + dtp$X19 + dtp$X20 + dtp$X21
dtp_4_ <- dtp$X5 + dtp$X6 + dtp$X7 + dtp$X8 + dtp$X9 + dtp$X10 + dtp$X11 + dtp$X12 + dtp$X13 + dtp$X14 + dtp$X15 + dtp$X16 + dtp$X17 + dtp$X18 + dtp$X19 + dtp$X20 + dtp$X21

dtp <- data.frame(dtp[,1:3], L_0_2 = dtp_0_2, L_2_4 = dtp_2_4, L_4_6 = dtp_4_6, L_6_10 = dtp_6_10, L_10_ = dtp_11_, L_4_ = dtp_4_)


#Convert pixels to percent of EAFB
#dtr <- t(apply(dtp[1:length(dtp[,1]),4:24], 1, function(x) round((x/total_area)*100,1)))
dtr <- t(apply(dtp[1:length(dtp[,1]),4:9], 1, function(x) round((x/total_area)*100,1)))

dt <- data.frame(dtp[,1:3], dtr)

#Convert rx fire scenarios to hectares (from acres)
dt$rx_fire[dt$rx_fire == 50] <- 20
dt$rx_fire[dt$rx_fire == 75] <- 30
dt$rx_fire[dt$rx_fire == 100] <- 40
dt$rx_fire[dt$rx_fire == 125] <- 50

#Generate a 3 - panel plot of fine fuel loading over time for low, medium, and high weight surface fuels
dev.off()
set.panel(3,1)
cf <- 0.5
scenario <- c(20,30,40,50)
ct <- as.character(c("blue", "green", "red", "pink"))

#Total fine fuel loading < 4.5 Mg/ha
i <- 4
par(tcl=-0.5, family="serif", mai=c(0.3,0.6,0.3,0.3))

for(a in 1:length(scenario))
  {
  ss <- which(dt$rx_fire == scenario[a])
  lc <- ct[a]
  min_ss <- seq(min(ss),max(ss),11)
  max_ss <- seq((min(ss)+10),max(ss),11)
  runs <- sort(unique(dt$run_no))
  for(b in 1:length(runs))
    {
    if(b == 1 & scenario[a] == scenario[1])
      {
      plot(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], type = "l", col = lc, ylim = c(30,55),
           ylab = "")
      } else
        {
          lines(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], col = lc)
        }
  }
}
lines(dt$sim_yr[min_ss[1]:max_ss[1]], rep(dt[min_ss[1],i],11), col = "black", lty = 2, lwd = 2)
legend(3,45, c("Baseline", expression("20k" ~ yr^-1), expression("30k" ~ yr^-1), 
               expression("30k" ~ yr^-1), expression("50k" ~ yr^-1)), 
       col = c("black", "blue", "green", "red", "pink"), 
       lty = c(2,1,1,1,1))
text(0, 53, "A)")
#title("Fine Fuel Loading Less Than 4.5 Mg/ha")

#Total fine fuel loading 4.5 - 9 Mg/ha
i <- 5
par(tcl=-0.5, family="serif", mai=c(0.3,0.6,0.3,0.3))

for(a in 1:length(scenario))
{
  ss <- which(dt$rx_fire == scenario[a])
  lc <- ct[a]
  min_ss <- seq(min(ss),max(ss),11)
  max_ss <- seq((min(ss)+10),max(ss),11)
  runs <- sort(unique(dt$run_no))
  for(b in 1:length(runs))
  {
    if(b == 1 & scenario[a] == scenario[1])
    {
      plot(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], type = "l", col = lc, ylim = c(10,35), 
           ylab = "Eglin Air Force Base - Percent Land Area")
    } else
    {
      lines(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], col = lc)
    }
  }
}
lines(dt$sim_yr[min_ss[1]:max_ss[1]], rep(dt[min_ss[1],i],11), col = "black", lty = 2, lwd = 2)
text(0, 33, "B)")
#title("Fine Fuel Loading 4.5-9.0 Mg/ha")

#Total fine fuel loading > 9 Mg/ha
i <- 9
par(tcl=-0.5, family="serif", mai=c(0.6,0.6,0.3,0.3))

for(a in 1:length(scenario))
{
  ss <- which(dt$rx_fire == scenario[a])
  lc <- ct[a]
  min_ss <- seq(min(ss),max(ss),11)
  max_ss <- seq((min(ss)+10),max(ss),11)
  runs <- sort(unique(dt$run_no))
  for(b in 1:length(runs))
  {
    if(b == 1 & scenario[a] == scenario[1])
    {
      plot(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], type = "l", col = lc, ylim = c(20,45), 
           xlab = "Simulation Year", ylab = "")
    } else
    {
      lines(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], col = lc)
    }
  }
}
lines(dt$sim_yr[min_ss[1]:max_ss[1]], rep(dt[min_ss[1],i],11), col = "black", lty = 2, lwd = 2)
text(0, 43, "C)")
#title("Fine Fuel Loading Greater Than 9.0 Mg/ha")

###############################################################################################################
###############################################################################################################
###############################################################################################################
#Generate a box plot of fine fuel loading change over time
dev.off()
#Remove scientific notation

#Subset data for every 10 years
dtx <- dt[dt[,3] %in% c(0,10,20,30,40,50),]

#Legend text
lt <- expression("Prescribed fire scenario (1000s of hectares" ~ year^-1 ~ "):  ")

########################################################################################################
#Panel A (less than 4.5 mg/ha)

#Subset data to only columns needed for boxplot.
dtya <- data.frame(rx_fire = as.factor(dtx[,1]),
                  sim_year = as.factor(dtx[,3]),
                  ff = dtx[,4])

#Set rx_fire scenarios to factors.
dtya$rx_fire <- factor(dtya$rx_fire , levels=c("20", "30", "40", "50"))

#Create a secondary data frame to plot boxes that will exclude baseline data
dtza <- dtya
#Convert baseline data to NA (so plots include sim year zero, but do not plot data for it).
dtza$ff[dtza$sim_year == 0] <- NA

#Reverse order of rx fire scenarios so they are listed in legend from lowest to highest.
dtza$rx_fire <- factor(dtza$rx_fire, levels = rev(levels(dtza$rx_fire)))

#Generate boxplot
bp1 <- ggplot(data = dtza, aes(x = sim_year, y = ff, fill = rx_fire))  +
  geom_boxplot(width = 0.75) + 
  labs(fill = lt) + 
  geom_hline(aes(yintercept = dtya$ff[dtya$sim_year == 0][1], linetype = "Baseline conditions")) + 
  scale_linetype_manual(name = "", values = "dashed") + 
  scale_fill_manual(values=c("50" = "pink", "40" = "red", 
                             "30" = "green", "20" = "blue")) + 
  annotate("text", x=0.75, y=54, label= "A)") + 
  theme(legend.title = element_text(color = "black", size = 9)) + 
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + 
  coord_cartesian(ylim = c(30, 55))

########################################################################################################
#Panel B (4.5 9.0 mg/ha)

#Subset data to only columns needed for boxplot.
dtyb <- data.frame(rx_fire = as.factor(dtx[,1]),
                  sim_year = as.factor(dtx[,3]),
                  ff = dtx[,5])

#Set rx_fire scenarios to factors.
dtyb$rx_fire <- factor(dtyb$rx_fire , levels=c("20", "30", "40", "50"))

#Create a secondary data frame to plot boxes that will exclude baseline data
dtzb <- dtyb
#Convert baseline data to NA (so plots include sim year zero, but do not plot data for it).
dtzb$ff[dtzb$sim_year == 0] <- NA

#Reverse order of rx fire scenarios so they are listed in legend from lowest to highest.
dtzb$rx_fire <- factor(dtzb$rx_fire, levels = rev(levels(dtzb$rx_fire)))

#Generate boxplot
bp2 <- ggplot(data = dtzb, aes(x = sim_year, y = ff, fill = rx_fire))  +
  geom_boxplot(width = 0.75) + 
  labs(fill = lt) + 
  geom_hline(aes(yintercept = dtyb$ff[dtyb$sim_year == 0][1], linetype = "Baseline conditions")) + 
  scale_linetype_manual(name = "", values = "dashed") + 
  scale_fill_manual(labels = c(expression("50k" ~ ha ~ yr^-1), expression("40k" ~ ha ~ yr^-1), 
                               expression("30k" ~ ha ~ yr^-1), expression("20k" ~ ha ~ yr^-1)), 
                    values=c("50" = "pink", "40" = "red", 
                             "30" = "green", "20" = "blue")) + 
  annotate("text", x=0.75, y=34, label= "B)") + 
  theme(legend.title = element_text(color = "black", size = 9)) + 
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + 
  coord_cartesian(ylim = c(10, 35))

########################################################################################################
#Panel C (greater than 9.0 mg/ha)

#Subset data to only columns needed for boxplot.
dtyc <- data.frame(rx_fire = as.factor(dtx[,1]),
                  sim_year = as.factor(dtx[,3]),
                  ff = dtx[,9])

#Set rx_fire scenarios to factors.
dtyc$rx_fire <- factor(dtyc$rx_fire , levels=c("20", "30", "40", "50"))

#Create a secondary data frame to plot boxes that will exclude baseline data
dtzc <- dtyc
#Convert baseline data to NA (so plots include sim year zero, but do not plot data for it).
dtzc$ff[dtzc$sim_year == 0] <- NA

#Reverse order of rx fire scenarios so they are listed in legend from lowest to highest.
dtzc$rx_fire <- factor(dtzc$rx_fire, levels = rev(levels(dtzc$rx_fire)))

#Generate boxplot
bp3 <- ggplot(data = dtzc, aes(x = sim_year, y = ff, fill = rx_fire))  +
  geom_boxplot(width = 0.75) + 
  labs(fill = lt) + 
  geom_hline(aes(yintercept = dtyc$ff[dtyc$sim_year == 0][1], linetype = "Baseline conditions")) + 
  scale_linetype_manual(name = "", values = "dashed") + 
  scale_fill_manual(values=c("50" = "pink", "40" = "red", 
                             "30" = "green", "20" = "blue")) + 
  annotate("text", x=0.75, y=44, label= "C)") + 
  theme(legend.title = element_text(color = "black", size = 9)) + 
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + 
  coord_cartesian(ylim = c(20, 45))


#Generate box plots
figure <- ggarrange(bp1, bp2, bp3,
                    common.legend = TRUE,
                    legend = "bottom",
                    ncol=1, 
                    nrow=3)

# Annotate the figure by adding a common labels
annotate_figure(figure,
                bottom = text_grob("Simulation Year", color = "black", size = 12),
                left = text_grob("Eglin Air Force Base - Percent Land Area", 
                                 color = "black", rot = 90, size = 12))

##############################################################################################################
##############################################################################################################
#SUMMARY STATS

#Total fine fuel loading < 4.5 Mg/ha
scenario <- c(20,30,40,50)
times <- c(0,10,20,30,40,50)


la <- matrix(data = 0, nrow = 24, ncol = 6)
la[,1] <- c(rep(20,6),rep(30,6),rep(40,6),rep(50,6))
la[,2] <- rep(c(0,10,20,30,40,50),4)
for(a in 1:length(scenario))
{
  for(b in 1:length(times))
  {
    la[,3][length(la[,3][la[,3] != 0])+1] <- round(mean(dt$L_0_2[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]]),1)
    la[,4][length(la[,4][la[,4] != 0])+1] <- round(sd(dt$L_0_2[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]]),2)
    la[,5][length(la[,5][la[,5] != 0])+1] <- min(dt$L_0_2[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]])
    la[,6][length(la[,6][la[,6] != 0])+1] <- max(dt$L_0_2[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]])
  }
}

#Total fine fuel loading 4.6 - 9.0 Mg/ha
scenario <- c(20,30,40,50)
times <- c(0,10,20,30,40,50)


ma <- matrix(data = 0, nrow = 24, ncol = 6)
ma[,1] <- c(rep(20,6),rep(30,6),rep(40,6),rep(50,6))
ma[,2] <- rep(c(0,10,20,30,40,50),4)
for(a in 1:length(scenario))
{
  for(b in 1:length(times))
  {
    ma[,3][length(ma[,3][ma[,3] != 0])+1] <- round(mean(dt$L_2_4[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]]),1)
    ma[,4][length(ma[,4][ma[,4] != 0])+1] <- round(sd(dt$L_2_4[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]]),2)
    ma[,5][length(ma[,5][ma[,5] != 0])+1] <- min(dt$L_2_4[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]])
    ma[,6][length(ma[,6][ma[,6] != 0])+1] <- max(dt$L_2_4[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]])
  }
}

#Total fine fuel loading > 9.0 Mg/ha
scenario <- c(20,30,40,50)
times <- c(0,10,20,30,40,50)


ha <- matrix(data = 0, nrow = 24, ncol = 6)
ha[,1] <- c(rep(20,6),rep(30,6),rep(40,6),rep(50,6))
ha[,2] <- rep(c(0,10,20,30,40,50),4)
for(a in 1:length(scenario))
{
  for(b in 1:length(times))
  {
    ha[,3][length(ha[,3][ha[,3] != 0])+1] <- round(mean(dt$L_4_[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]]),1)
    ha[,4][length(ha[,4][ha[,4] != 0])+1] <- round(sd(dt$L_4_[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]]),2)
    ha[,5][length(ha[,5][ha[,5] != 0])+1] <- min(dt$L_4_[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]])
    ha[,6][length(ha[,6][ha[,6] != 0])+1] <- max(dt$L_4_[dt$rx_fire == scenario[a] & dt$sim_yr == times[b]])
  }
}
