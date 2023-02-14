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

#Set working directory
setwd(paste(usfs, "Documents/FDM_2023_Simulation_Data/Step_02c_Disturbance_Summary_Table", sep = ""))

#Open data
st <- read.csv("disturbance_summary_table.csv", header=TRUE, 
               sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Set up loop to cycle through all simulation feeds.
scenario <- c(50,75,100,125)
run <- 1:10
sim_years <- 1:50

#Set up inputs for plotting
dev.off()


ct <- as.character(c("blue", "green", "orange", "pink"))
options(scipen = 999)
d_cols <- c(7,8,9,10)

tfi <- layout(matrix(c(1,2,3,1,4,5,6,7,7),3,3,byrow=TRUE), 
              c(1,17.5,17.5), c(12.8,12.8,1.0),
              TRUE)
#Text position
tp <- 0.95 #Percent from top

#Box 1 - "y axis"
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1,5,"Disturbance area (x1000 ha)", pos=3, srt=90)

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
           ylim = c(1.6, 2.8), ylab = "", yaxt = "n")
      axis(2, at = c(1.6, 2.0, 2.4, 2.8))
      text(1, (2.8 * tp), "C)")
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

legend(31,1.9, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "orange", "pink"), 
       lty = 1)

#Box 6 - nothing
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1,6,"",pos=3,srt=90)

#Box 7 - "x axis"
par(mar=c(0,0,0,0),cex=1,family="serif")
plot(1,1,pch=1,col="white",xlim=c(0,2),ylim=c(0,10),xaxt="n",yaxt="n",bty="n")
text(1.25,6,"Simulation Year",adj=1)



#####################################################################################################################













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
legend(40,40000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "orange", "pink"), 
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
legend(40,15000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "orange", "pink"), 
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
legend(40,15000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "orange", "pink"), 
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
legend(40,40000, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "orange", "pink"), 
       lty = 1)

