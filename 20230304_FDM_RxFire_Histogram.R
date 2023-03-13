#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script generates histograms for rx fire stats.
#GitHub Repository: FDM_Output_Analysis

#Author: Jim Cronan
#Organization: US Forest Service
#Address:
#400 N 34th Street
#Suite 201
#Seattle, WA 98103
#Date Created: 4-Mar-2023

library(stringr)#needed for str_extract
library(fields)#for set.panel()

#################################################################################################
#################################################################################################
#DATA INPUTS

#Which computer are you using?
#USFS
usfs <- "C:/Users/jcronan/OneDrive - USDA/"
pers <- "C:/Users/james/"

#Set working directory
setwd(paste(pers, "Documents/FDM_2023_Simulation_Data/Step_02c_Disturbance_Summary_Table", sep = ""))

#Open data
st <- read.csv("disturbance_RxFire_compiled.csv", header=TRUE, 
               sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#################################################################################################
#################################################################################################
##PERCENT UNIT BURNED HISTOGRAMS

#Calculate ratio of unit burned for each prescibed fire
perc_burned <- round((st$actual_rx_fire_area/st$admin_rx_fire_area)*100,1)
perc_burned[is.na(perc_burned) == T] <- 0
perc_burned[perc_burned == Inf] <- 100

#break points
breaks <- seq(0,100,5)

#Generate panes for plots
tfi <- layout(matrix(c(1,2,3,1,4,5,1,6,7,1,8,9,10,11,11),5,3,byrow=TRUE), 
              c(1,17.5,17.5), c(6.4,6.4,6.4,6.4,1.0),
              TRUE)

#Box 1 - "y axis"
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1,5,"Number of prescribed fires", pos=3, srt=90)

#Box 2 - Data (50 acres/yr | sim years 1-5)
par(tcl=-0.5, family="serif", mai=c(0.4,0.5,0.3,0.3))

#Common plotting variables
cx <- 0.75
ad <- 0
a1 <- 0.95
a2 <- 0.8
ma <- 0.01
ya <- c(0,2500)

aab <- 50
sy <- 1:5
data <- perc_burned[st$scenario == aab & st$sim_year %in% sy]

hist(data, breaks = breaks, main = "", ylim = ya, 
     xlab = "", ylab = "")
abline(v = median(data),                       # Add line for mean
       col = "black",
       lwd = 3)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a1,
     paste("A)", sep = ""),
     col = "black",
     cex = cx,
     adj = ad)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a2,
     paste("Median: ", round(median(data),1), "%", sep = ""),
     col = "black",
     cex = cx,
     adj = ad)

#Box 3 - Data (50 acres/yr | sim years 45-50)
par(tcl=-0.5, family="serif", mai=c(0.4,0.5,0.3,0.3))

aab <- 50
sy <- 45:50
data <- perc_burned[st$scenario == aab & st$sim_year %in% sy]

hist(data, breaks = breaks, main = "", ylim = ya, 
     xlab = "", ylab = "")
abline(v = median(data),                       # Add line for mean
       col = "black",
       lwd = 3)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a1,
     paste("B)", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] *a2,
     paste("Median: ", round(median(data),1), "%", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)

#Box 4 - Data (75 acres/yr | sim years 1-5)
par(tcl=-0.5, family="serif", mai=c(0.4,0.5,0.3,0.3))

aab <- 75
sy <- 1:5
data <- perc_burned[st$scenario == aab & st$sim_year %in% sy]

hist(data, breaks = breaks, main = "", ylim = ya, 
     xlab = "", ylab = "")
abline(v = median(data),                       # Add line for mean
       col = "black",
       lwd = 3)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a1,
     paste("C)", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a2,
     paste("Median: ", round(median(data),1), "%", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)

#Box 5 - Data (75 acres/yr | sim years 45-50)
par(tcl=-0.5, family="serif", mai=c(0.4,0.5,0.3,0.3))

aab <- 75
sy <- 45:50
data <- perc_burned[st$scenario == aab & st$sim_year %in% sy]

hist(data, breaks = breaks, main = "", ylim = ya, 
     xlab = "", ylab = "")
abline(v = median(data),                       # Add line for mean
       col = "black",
       lwd = 3)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a1,
     paste("D)", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a2,
     paste("Median: ", round(median(data),1), "%", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)

#Box 6 - Data (100 acres/yr | sim years 1-5)
par(tcl=-0.5, family="serif", mai=c(0.4,0.5,0.3,0.3))

aab <- 100
sy <- 1:5
data <- perc_burned[st$scenario == aab & st$sim_year %in% sy]

hist(data, breaks = breaks, main = "", ylim = ya, 
     xlab = "", ylab = "")
abline(v = median(data),                       # Add line for mean
       col = "black",
       lwd = 3)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a1,
     paste("E)", sep = ""),
     col = "black",
     cex = cx,
     adj = ad)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a2,
     paste("Median: ", round(median(data),1), "%", sep = ""),
     col = "black",
     cex = cx,
     adj = ad)

#Box 7 - Data (100 acres/yr | sim years 45-50)
par(tcl=-0.5, family="serif", mai=c(0.4,0.5,0.3,0.3))

aab <- 100
sy <- 45:50
data <- perc_burned[st$scenario == aab & st$sim_year %in% sy]

hist(data, breaks = breaks, main = "", ylim = ya, 
     xlab = "", ylab = "")
abline(v = median(data),                       # Add line for mean
       col = "black",
       lwd = 3)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a1,
     paste("F)", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] *a2,
     paste("Median: ", round(median(data),1), "%", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)

#Box 8 - Data (125 acres/yr | sim years 1-5)
par(tcl=-0.5, family="serif", mai=c(0.4,0.5,0.3,0.3))

aab <- 125
sy <- 1:5
data <- perc_burned[st$scenario == aab & st$sim_year %in% sy]

hist(data, breaks = breaks, main = "", ylim = ya, 
     xlab = "", ylab = "")
abline(v = median(data),                       # Add line for mean
       col = "black",
       lwd = 3)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a1,
     paste("G)", sep = ""),
     col = "black",
     cex = cx,
     adj = ad)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a2,
     paste("Median: ", round(median(data),1), "%", sep = ""),
     col = "black",
     cex = cx,
     adj = ad)

#Box 9 - Data (125 acres/yr | sim years 45-50)
par(tcl=-0.5, family="serif", mai=c(0.4,0.5,0.3,0.3))

aab <- 125
sy <- 45:50
data <- perc_burned[st$scenario == aab & st$sim_year %in% sy]

hist(data, breaks = breaks, main = "", ylim = ya, 
     xlab = "", ylab = "")
abline(v = median(data),                       # Add line for mean
       col = "black",
       lwd = 3)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] * a1,
     paste("H)", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)
text(x = median(data) * ma,                   # Add text for mean
     y = ya[2] *a2,
     paste("Median: ", round(median(data),1), "%", sep = ""),
     col = "black",
     cex = cx, 
     adj = ad)

#Box 10 - nothing
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1,6,"",pos=3,srt=90)

#Box 11 - "x axis"
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1.2,6,"Percent of unit burned",adj=1)

#################################################################################################
#################################################################################################
#PERCENT UNIT BURNED HISTOGRAMS AND ADDITIONAL STATS
aab <- 125
sy <- 45:50

median(perc_burned[st$scenario == aab & st$sim_year %in% sy])

#################################################################################################
#################################################################################################
#END
