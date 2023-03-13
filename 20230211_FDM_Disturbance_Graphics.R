#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script generates graphics for simulated disturbance metrics.
#GitHub Repository: FDM_Output_Analysis

#Author: Jim Cronan
#Organization: US Forest Service
#Address:
#400 N 34th Street
#Suite 201
#Seattle, WA 98103
#Date Created: 11-Feb-2023

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
st <- read.csv("disturbance_summary_table.csv", header=TRUE, 
               sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Convert rx fire scenarios to hectares (from acres)
st$scenario[st$scenario == 50] <- 20
st$scenario[st$scenario == 75] <- 30
st$scenario[st$scenario == 100] <- 40
st$scenario[st$scenario == 125] <- 50

#Summary stats - prescribed fires
s_years <- 2:50
sc <- 20
round(median(st$admin_rx_fire_area[st$scenario == sc & st$sim_year %in% s_years]) * 0.404686, 0)
round(range(st$admin_rx_fire_area[st$scenario == sc & st$sim_year %in% s_years]) * 0.404686, 0)

sc <- 30
round(median(st$admin_rx_fire_area[st$scenario == sc & st$sim_year %in% s_years]) * 0.404686, 0)
round(range(st$admin_rx_fire_area[st$scenario == sc & st$sim_year %in% s_years]) * 0.404686, 0)

sc <- 40
round(median(st$admin_rx_fire_area[st$scenario == sc & st$sim_year %in% s_years]) * 0.404686, 0)
round(range(st$admin_rx_fire_area[st$scenario == sc & st$sim_year %in% s_years]) * 0.404686, 0)

sc <- 50
round(median(st$admin_rx_fire_area[st$scenario == sc & st$sim_year %in% s_years]) * 0.404686, 0)
round(range(st$admin_rx_fire_area[st$scenario == sc & st$sim_year %in% s_years]) * 0.404686, 0)

#Summary stats - wildfires
head(st)

#Set up loop to cycle through all simulation feeds.
scenario <- c(20,30,40,50)
run <- 1:10
sim_years <- 1:50

#Eglin Air Force Base
#Expected annual area burned in wildfires
#467,086 acres
#432,878 acres (1,946,463 pixels) are vegetated terrestrial.
#Fire rotation: 54.4 years
(467086/54.4) * 0.404686
103.7 * 0.404686
432878/54.4

aab_wfr <- st$eglin_wildfire_area + st$eglin_blockburn_area
round(mean(aab_wfr[st$scenario == 20]) * 0.404686,0)
round(mean(aab_wfr[st$scenario == 30]) * 0.404686,0)
round(mean(aab_wfr[st$scenario == 40]) * 0.404686,0)
round(mean(aab_wfr[st$scenario == 50]) * 0.404686,0)

#10-km Buffer Zone
#Expected annual area burned in wildfires
#Area of buffer zone is 580,925.2 acres
#Area in hectares
bza <- 580925.2  * 0.404686
#Fire rotation: 457.4 years
(bza/457.4)#average annual area needed to burn for this fire rotation

aab_wfr <- st$buffer_wildfire_area
round(mean(aab_wfr[st$scenario == 20]) * 0.404686,0)
round(mean(aab_wfr[st$scenario == 30]) * 0.404686,0)
round(mean(aab_wfr[st$scenario == 40]) * 0.404686,0)
round(mean(aab_wfr[st$scenario == 50]) * 0.404686,0)
round(mean(aab_wfr) * 0.404686,0)

#Pre-commercial thinning
pct_t <- st$trtd_pct_area
pct_a <- st$admin_pct_area

round(mean(pct_a[st$scenario == 20]) * 0.404686,0)
round(mean(pct_a[st$scenario == 30]) * 0.404686,0)
round(mean(pct_a[st$scenario == 40]) * 0.404686,0)
round(mean(pct_a[st$scenario == 50]) * 0.404686,0)
round(mean(pct_a) * 0.404686,0)

#Years 1-25
round(mean(pct_a[st$scenario == 20 & st$sim_year %in% 1:25]) * 0.404686,0)
round(mean(pct_a[st$scenario == 30 & st$sim_year %in% 1:25]) * 0.404686,0)
round(mean(pct_a[st$scenario == 40 & st$sim_year %in% 1:25]) * 0.404686,0)
round(mean(pct_a[st$scenario == 50 & st$sim_year %in% 1:25]) * 0.404686,0)
round(mean(pct_a[st$sim_year %in% 1:25]) * 0.404686,0)

#Years 26-50
round(mean(pct_a[st$scenario == 20 & st$sim_year %in% 26:50]) * 0.404686,0)
round(mean(pct_a[st$scenario == 30 & st$sim_year %in% 26:50]) * 0.404686,0)
round(mean(pct_a[st$scenario == 40 & st$sim_year %in% 26:50]) * 0.404686,0)
round(mean(pct_a[st$scenario == 50 & st$sim_year %in% 26:50]) * 0.404686,0)
round(mean(pct_a[st$sim_year %in% 26:50]) * 0.404686,0)

#Years 1-25
round(max(pct_a[st$scenario == 20 & st$sim_year %in% 1:25]) * 0.404686,0)
round(max(pct_a[st$scenario == 30 & st$sim_year %in% 1:25]) * 0.404686,0)
round(max(pct_a[st$scenario == 40 & st$sim_year %in% 1:25]) * 0.404686,0)
round(max(pct_a[st$scenario == 50 & st$sim_year %in% 1:25]) * 0.404686,0)
round(max(pct_a[st$sim_year %in% 1:25]) * 0.404686,0)

#Years 26-50
round(max(pct_a[st$scenario == 20 & st$sim_year %in% 26:50]) * 0.404686,0)
round(max(pct_a[st$scenario == 30 & st$sim_year %in% 26:50]) * 0.404686,0)
round(max(pct_a[st$scenario == 40 & st$sim_year %in% 26:50]) * 0.404686,0)
round(max(pct_a[st$scenario == 50 & st$sim_year %in% 26:50]) * 0.404686,0)
round(max(pct_a[st$sim_year %in% 26:50]) * 0.404686,0)

ra <- pct_t/pct_a
mean(ra)
range(ra)

#Herbicide
h_t <- st$trtd_herbicide_area
h_a <- st$admin_herbicide_area

round(mean(h_a[st$scenario == 20]) * 0.404686,0)
round(mean(h_a[st$scenario == 30]) * 0.404686,0)
round(mean(h_a[st$scenario == 40]) * 0.404686,0)
round(mean(h_a[st$scenario == 50]) * 0.404686,0)
round(mean(h_a) * 0.404686,0)

#Year 1
round(mean(h_a[st$scenario == 20 & st$sim_year == 1]) * 0.404686,0)
round(mean(h_a[st$scenario == 30 & st$sim_year == 1]) * 0.404686,0)
round(mean(h_a[st$scenario == 40 & st$sim_year == 1]) * 0.404686,0)
round(mean(h_a[st$scenario == 50 & st$sim_year == 1]) * 0.404686,0)
round(mean(h_a[st$sim_year == 1]) * 0.404686,0)

#Years 25
round(mean(h_a[st$scenario == 20 & st$sim_year == 25]) * 0.404686,0)
round(mean(h_a[st$scenario == 30 & st$sim_year == 25]) * 0.404686,0)
round(mean(h_a[st$scenario == 40 & st$sim_year == 25]) * 0.404686,0)
round(mean(h_a[st$scenario == 50 & st$sim_year == 25]) * 0.404686,0)
round(mean(h_a[st$sim_year == 25]) * 0.404686,0)

ra <- h_t/h_a
mean(ra)
range(ra)

#################################################################################################
#################################################################################################
#LINE PLOTS

#Set up inputs for plotting
dev.off()

#Set colors for each scenario
ct <- as.character(c("blue", "green", "red", "pink"))

#Generate panes for plots
tfi <- layout(matrix(c(1,2,3,1,4,5,6,7,7),3,3,byrow=TRUE), 
              c(1,17.5,17.5), c(12.8,12.8,1.0),
              TRUE)
#Text position
tp <- 0.95 #Percent from top

#Box 1 - "y axis"
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1,5,"Disturbance area (1000s of hectares)", pos=3, srt=90)

#Box 2 - Prescribed fire
par(tcl=-0.5, family="serif", mai=c(0.3,0.5,0.3,0.3))

#Reduce by 1000 and convert to hectares
svr <- (st[,9]/1000) * 0.404686
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], svr[st$run == b & st$scenario == scenario[a]], type = "l", col = lc, 
           ylim = c(0,60), ylab = "", yaxt = "n")
      axis(2, at = c(0, 20, 40, 60))
      text(1, (60 * tp), "A)")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], svr[st$run == b & st$scenario == scenario[a]], col = lc)
    }
  }
}


#Box 3 - Wildfire - combined
par(tcl=-0.5, family="serif", mai=c(0.3,0.3,0.3,0.3))

#Reduce by 1000 and convert to hectares
wfc <- ((st[,10] + st[,11])/1000) * 0.404686
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], wfc[st$run == b & st$scenario == scenario[a]], type = "l", col = lc, 
           ylim = c(0,15), ylab = "", yaxt = "n")
      axis(2, at = c(0, 5, 10, 15))
      text(1, (15 * tp), "B)")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], wfc[st$run == b & st$scenario == scenario[a]], col = lc)
    }
  }
}

#Box 4 - Precommercial thinning
par(tcl=-0.5, family="serif", mai=c(0.5,0.5,0.3,0.3))

#Reduce by 1000 and convert to hectares
svr <- (st[,7]/1000) * 0.404686
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], svr[st$run == b & st$scenario == scenario[a]], type = "l", col = lc, 
           ylim = c(1.8, 3.0), ylab = "", yaxt = "n")
      axis(2, at = c(1.8, 2.2, 2.6, 3.0))
      text(1, (3.1 * tp), "C)")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], svr[st$run == b & st$scenario == scenario[a]], col = lc)
    }
  }
}

#Box 5 - Herbicide
par(tcl=-0.5, family="serif", mai=c(0.5,0.3,0.3,0.3))

#Reduce by 1000 and convert to hectares
svr <- (st[,8]/1000) * 0.404686
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], svr[st$run == b & st$scenario == scenario[a]], type = "l", col = lc, 
           ylim = c(0.5,3.2), ylab = "", yaxt = "n")
      axis(2, at = c(0.8, 1.6, 2.4, 3.2))
      text(1, (3.2 * tp), "D)")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], svr[st$run == b & st$scenario == scenario[a]], col = lc)
    }
  }
}

legend(31,1.8, c(expression("20k" ~ yr^-1), expression("30k" ~ yr^-1), 
               expression("30k" ~ yr^-1), expression("50k" ~ yr^-1)), 
       col = c("blue", "green", "red", "pink"), 
       lty = 1)

#Box 6 - nothing
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1,6,"",pos=3,srt=90)

#Box 7 - "x axis"
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1.1,6,"Simulation Year",adj=1)



#####################################################################################################################
#Extra plots

#Prescribed fire
dev.off()
i <- 9
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], st[st$run == b & st$scenario == scenario[a],i], type = "l", col = lc, 
           ylim = c(0,160000), ylab = "")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], st[st$run == b & st$scenario == scenario[a],i], col = lc)
    }
  }
}
legend(40,40000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "red", "pink"), 
       lty = 1)

#Wildfire - natural spread
dev.off()
i <- 10
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], st[st$run == b & st$scenario == scenario[a],i], type = "l", col = lc, 
           ylim = c(0,20000), ylab = "")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], st[st$run == b & st$scenario == scenario[a],i], col = lc)
    }
  }
}
legend(40,15000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "red", "pink"), 
       lty = 1)

#Wildfire - block and burn
dev.off()
i <- 11
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], st[st$run == b & st$scenario == scenario[a],i], type = "l", col = lc, 
           ylim = c(0,20000), ylab = "")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], st[st$run == b & st$scenario == scenario[a],i], col = lc)
    }
  }
}
legend(40,15000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "red", "pink"), 
       lty = 1)


#all fire (wildfire and prescribed)
afc <- st[,9] + st[,10] + st[,11]
dev.off()
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], afc[st$run == b & st$scenario == scenario[a]], type = "l", col = lc, 
           ylim = c(0,160000), ylab = "")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], afc[st$run == b & st$scenario == scenario[a]], col = lc)
    }
  }
}
legend(40,40000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "red", "pink"), 
       lty = 1)

#herbicide (actual:admin area)
hr <- st[,5]/st[,8]
dev.off()
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], hr[st$run == b & st$scenario == scenario[a]], 
           type = "l", col = lc, 
           ylim = c(0,1), ylab = "")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], hr[st$run == b & st$scenario == scenario[a]], 
            col = lc)
    }
  }
}
legend(40,40000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "red", "pink"), 
       lty = 1)

#pct (actual:admin area)
tr <- st[,4]/st[,7]
dev.off()
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], tr[st$run == b & st$scenario == scenario[a]], 
           type = "l", col = lc, 
           ylim = c(0,1), ylab = "")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], tr[st$run == b & st$scenario == scenario[a]], 
            col = lc)
    }
  }
}
legend(40,40000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "red", "pink"), 
       lty = 1)

#rx fire (actual:admin area)
fr <- st[,6]/st[,9]
dev.off()
for(a in 1:length(scenario))
{
  lc <- ct[a]
  for(b in run)
  {
    if(a == 1 & b == 1)
    {
      plot(st$sim_year[st$run == b & st$scenario == scenario[a]], fr[st$run == b & st$scenario == scenario[a]], 
           type = "l", col = lc, 
           ylim = c(0,1), ylab = "")
    } else
    {
      lines(st$sim_year[st$run == b & st$scenario == scenario[a]], fr[st$run == b & st$scenario == scenario[a]], 
            col = lc)
    }
  }
}
legend(40,40000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "red", "pink"), 
       lty = 1)

#################################################################################################
#################################################################################################
#SUMMARY STATS/INFO FOR RESULTS
head(st)



0.404686
