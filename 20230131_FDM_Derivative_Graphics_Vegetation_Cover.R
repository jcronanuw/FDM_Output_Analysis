#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script generates graphics from tabular data for vegetation cover tables produced
#in 20230131_FDM_Derivative_Processing_Fine_Fuels.R scrip in this GitHub repository.
#GitHub Repository: FDM_Output_Analysis
#Graphics:
#1) Line graphs
#2) Box plots

#Author: jim Cronan
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

#Look at totals for each row. This is the number of pixels in each map. These numbers should
#all be the same, or at least in a very narrow range.
dt_sum <- apply(dt_csv[,4:13],1,sum)
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

#Convert pixels to percent of EAFB
#dtr <- t(apply(dtp[1:length(dtp[,1]),4:24], 1, function(x) round((x/total_area)*100,1)))
dtr <- t(apply(dtp[1:length(dtp[,1]),4:13], 1, function(x) round((x/total_area)*100,1)))

dt <- data.frame(dtp[,1:3], dtr)

#Convert rx fire scenarios to hectares (from acres)
dt$rx_fire[dt$rx_fire == 50] <- 20
dt$rx_fire[dt$rx_fire == 75] <- 30
dt$rx_fire[dt$rx_fire == 100] <- 40
dt$rx_fire[dt$rx_fire == 125] <- 50

dev.off()
#Generate a 3 - panel plot of fine fuel loading over time for low, medium, and high weight surface fuels
tfi <- layout(matrix(c(1,2,3,1,4,5,1,6,7,8,9,9),4,3,byrow=TRUE), 
              c(1,17.5,17.5), c(8.5,8.5,8.5,1.1),
              TRUE)
cf <- 0.5

#1 - long needle pine
#2 - mixed broadleaf pine
#3 - broadleaf
#4 - short-needle pine
#5 - tall shrub
scenario <- c(20,30,40,50)
ct <- as.character(c("blue", "green", "red", "pink"))


#Box 1 - "y axis"
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1,5,"Eglin Air Force Base - Percent Land Area", pos=3, srt=90)

#Box 2 - Long-needle pine
i <- 5
par(tcl=-0.5, family="serif", mai=c(0.3,0.3,0.3,0.3))
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
      plot(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], type = "l", col = lc, ylim = c(20,70),
           ylab = "")
      } else
        {
          lines(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], col = lc)
        }
  }
}
lines(dt$sim_yr[min_ss[1]:max_ss[1]], rep(dt[min_ss[1],i],11), col = "black", lty = 2, lwd = 2)

text(0, 68, "A)")

#Box 3 - Legend
par(tcl=-0.5, family="serif", mai=c(0.3,0.3,0.3,0.3))
plot(1, 1, type = "l", col = lc, xlim = c(0,100), ylim = c(0,100), xaxt = 'n', labels = F, yaxt = 'n', bty = "n", 
     xlab = "", ylab = "")
par(mar=c(0,0,0,0),cex=1,family="serif")
legend(35,90, c("Baseline", expression("20k" ~ yr^-1), expression("30k" ~ yr^-1), 
                expression("30k" ~ yr^-1), expression("50k" ~ yr^-1)), col = c("black", "blue", "green", "red", "pink"), 
       lty = c(2,1,1,1,1))

#Box 4 - Mixed broadleaf-pine
i <- 6
par(tcl=-0.5, family="serif", mai=c(0.3,0.3,0.3,0.3))

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
      plot(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], type = "l", col = lc, ylim = c(0,50), 
           ylab = "")
    } else
    {
      lines(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], col = lc)
    }
  }
}
lines(dt$sim_yr[min_ss[1]:max_ss[1]], rep(dt[min_ss[1],i],11), col = "black", lty = 2, lwd = 2)
text(0, 48, "B)")


#Box 5 - Broadleaf
i <- 7
par(tcl=-0.5, family="serif", mai=c(0.3,0.3,0.3,0.3))

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
      plot(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], type = "l", col = lc, ylim = c(0,50), 
           xlab = "", ylab = "")
    } else
    {
      lines(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], col = lc)
    }
  }
}
lines(dt$sim_yr[min_ss[1]:max_ss[1]], rep(dt[min_ss[1],i],11), col = "black", lty = 2, lwd = 2)
text(0, 48, "C)")

#Box 6 - Short-needle pine
i <- 8
par(tcl=-0.5, family="serif", mai=c(0.3,0.3,0.3,0.3))

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
      plot(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], type = "l", col = lc, ylim = c(0,50), 
           xlab = "", ylab = "")
    } else
    {
      lines(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], col = lc)
    }
  }
}
lines(dt$sim_yr[min_ss[1]:max_ss[1]], rep(dt[min_ss[1],i],11), col = "black", lty = 2, lwd = 2)
text(0, 48, "D)")

#Box 7 - Tall shrub
i <- 9
par(tcl=-0.5, family="serif", mai=c(0.3,0.3,0.3,0.3))

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
      plot(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], type = "l", col = lc, ylim = c(0,50), 
           xlab = "", ylab = "")
    } else
    {
      lines(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], col = lc)
    }
  }
}
lines(dt$sim_yr[min_ss[1]:max_ss[1]], rep(dt[min_ss[1],i],11), col = "black", lty = 2, lwd = 2)
text(0, 48, "E)")

#Box 8 - nothing
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1,6,"",pos=3,srt=90)

#Box 9 - "x axis"
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1.1,4,"Simulation Year",adj=1)

###############################################################################################################
###############################################################################################################
###############################################################################################################
#Generate a box plot of fine fuel loading change over time
dev.off()
#Remove scientific notation

#Subset data for every 10 years
dtx <- dt[dt[,3] %in% c(0,10,20,30,40,50),]

#Convert acres to hectares
dtx[,1] <- floor((dtx[,1] * 0.404686))

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
  labs(fill="Prescribed fire scenario\n(1000s of hectares)") + 
  geom_hline(aes(yintercept = dtya$ff[dtya$sim_year == 0][1], linetype = "Baseline conditions")) + 
  scale_linetype_manual(name = "", values = "dashed") + 
  scale_fill_manual(values=c("50" = "pink", "40" = "red", 
                             "30" = "green", "20" = "blue")) + 
  annotate("text", x=0.75, y=51, label= "A)") + 
  theme(legend.position = "none") + 
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank())

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
  geom_boxplot(width = 0.75) + ylab("Eglin Air Force Base - Percent Land Area") + 
  labs(fill="Prescribed fire scenario\n(1000s of hectares)") + 
  geom_hline(aes(yintercept = dtyb$ff[dtyb$sim_year == 0][1], linetype = "Baseline conditions")) + 
  scale_linetype_manual(name = "", values = "dashed") + 
  scale_fill_manual(labels = c(expression("50k" ~ ha ~ yr^-1), expression("40k" ~ ha ~ yr^-1), 
                               expression("30k" ~ ha ~ yr^-1), expression("20k" ~ ha ~ yr^-1)), 
                    values=c("50" = "pink", "40" = "red", 
                             "30" = "green", "20" = "blue")) + 
  annotate("text", x=0.75, y=26, label= "B)") + 
  theme(axis.title.x=element_blank())

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
  geom_boxplot(width = 0.75) + xlab("Simulation Year") + 
  labs(fill="Prescribed fire scenario\n(1000s of hectares)") + 
  geom_hline(aes(yintercept = dtyc$ff[dtyc$sim_year == 0][1], linetype = "Baseline conditions")) + 
  scale_linetype_manual(name = "", values = "dashed") + 
  scale_fill_manual(values=c("50" = "pink", "40" = "red", 
                             "30" = "green", "20" = "blue")) + 
  annotate("text", x=0.75, y=41, label= "C)") + 
  theme(legend.position = "none") + 
  theme(axis.title.y=element_blank())


#Plot box plots
figure<- ggarrange(bp1,bp2,bp3,
                   ncol=1,nrow=3)

