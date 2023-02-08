
#################################################################################################
#################################################################################################
#DATA INPUTS

#################################################################################################
setwd(paste("E:/FDM_2023_Simulation_Data/Step_05_Derivative_Tables", sep = ""))

#Import input parameters
v1 <- read.csv("Derivative_table_fine_fuels.csv", header=TRUE, 
                   sep=",", na.strings="NA", dec=".", strip.white=TRUE)

v2 <- read.csv("v4_Derivative_table_fine_fuels.csv", header=TRUE, 
               sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Look at totals for each row. This is the number of pixels in each map. These numbers should
#all be the same, or at least in a very narrow range.
test <- apply(v2[,3:24],1,sum)
range(test)
plot(test)
total_pixels <- mean(test)
#They are not the same but they fall in a narrow range: 1,946,438 - 1,946,488
#Whats more the range is the same for every simulation with pixel count
#steadily increasing as the simulation years progress. Not sure what could be causing this,
#possibly a slight, but consistent shift in coverage of the mask?
#At any rate this should have no effect on the data.

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
dtp <- v2

dtp_0_2 <- dtp$X1 + dtp$X2
dtp_2_4 <- dtp$X3 + dtp$X4
dtp_4_6 <- dtp$X5 + dtp$X6
dtp_6_10 <- dtp$X7 + dtp$X8 + dtp$X9 + dtp$X10
dtp_11_ <- dtp$X11 + dtp$X12 + dtp$X13 + dtp$X14 + dtp$X15 + dtp$X16 + dtp$X17 + dtp$X18 + dtp$X19 + dtp$X20 + dtp$X21
dtp_6_ <- dtp$X7 + dtp$X8 + dtp$X9 + dtp$X10 + dtp$X11 + dtp$X12 + dtp$X13 + dtp$X14 + dtp$X15 + dtp$X16 + dtp$X17 + dtp$X18 + dtp$X19 + dtp$X20 + dtp$X21

dtp <- data.frame(dtp[,1:3], L_0_2 = dtp_0_2, L_2_4 = dtp_2_4, L_4_6 = dtp_4_6, L_6_10 = dtp_6_10, L_10_ = dtp_11_, L_6_ = dtp_6_)


#Convert pixels to percent of EAFB
#dtr <- t(apply(dtp[1:length(dtp[,1]),4:24], 1, function(x) round((x/total_area)*100,1)))
dtr <- t(apply(dtp[1:length(dtp[,1]),4:9], 1, function(x) round((x/total_area)*100,1)))

dt <- data.frame(dtp[,1:3], dtr)

i <- 9
scenario <- c(50,75,100,125)
ct <- as.character(c("blue", "green", "orange", "red"))

for(a in 1:length(scenario))
  {
  ss <- which(dt$rx_fire == scenario[a])
  lc <- ct[a]
  min_ss <- seq(min(ss),max(ss),11)
  max_ss <- seq((min(ss)+10),max(ss),11)
  runs <- sort(unique(dt$run_no))
  for(b in 1:length(runs))
    {
    if(b == 1 & scenario[a] == 50)
      {
      plot(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], type = "l", col = lc, ylim = c(0,max(dt[,4])), 
         xlab = "Simulation Year", ylab = "Fine Fuel Load (t/ac)")
      } else
        {
          lines(dt$sim_yr[min_ss[b]:max_ss[b]], dt[min_ss[b]:max_ss[b],i], col = lc)
        }
  }
}
lines(dt$sim_yr[min_ss[1]:max_ss[1]], rep(dt[min_ss[1],i],11), col = "black", lty = 2, lwd = 2)


dev.off()




dev.off()


dev.off()

legend(5,60, c("50k/yr", "75k/yr", "100k/yr", "125k/yr"), col = c("blue", "green", "orange", "red"), lty = 1)




