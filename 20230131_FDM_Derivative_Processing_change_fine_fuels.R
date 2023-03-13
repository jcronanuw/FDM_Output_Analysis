#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE: This script generates tabular data for the change in fine fuel loading from
#FDM FCCS secondary maps (primary are the fuelbed maps, seconday are the fuelbed
#derivative maps)

#1) 125% of current -- 125k acres prescribed burned/year
#2) 100% of current -- 100k acres prescribed burned/year
#3) 75% of current -- 75k acres prescribed burned/year
#4) 50% of current -- 50k acres prescribed burned/year

#Author: Jim Cronan
#Organization: US Forest Service
#Address: 
#Pacific Wildland Fire Sciences Laboratory
#400 North 34th Street
#Suite 201
#Seattle, WA 98103
#Date Created: January 31, 2023

#################################################################################################
#################################################################################################
#LIBRARIES

#Libraries
library(raster)

#Name of external drive
ed <- "E"

#Which computer are you using
#Dell desktop (1) or USFS laptop (2)
comp <- "C:/Users/jcron/"
#comp <- "C:/Users/jcronan/OneDrive - USDA/"

#################################################################################################
#################################################################################################
#DATA INPUTS

#################################################################################################
setwd(paste(ed, ":/FDM_2023_Simulation_Data/Step_02_Fuelbed_Derivative_Maps/maps", sep = ""))

#Import a single raster file to use header data to reference number of columns for matrix(scan())
f.head <- raster("c050_01_00.asc")
maps_orig <- matrix(scan("c050_01_00.asc",skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
#50K/YEAR RUN

#Set up a list to hold input and output maps
rx_fire <- c("050", "075", "100", "125")
run_number <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")

#Output intervals
intervals <- c("05", as.character(seq(10,50,5)))

#Input file pre-fix
prefix_in <- "c"


prefix_name <- "change_fine_fuels"

#Open the map with prescribed fire burn units. This is needed to remove the buffer zone from the analysis area
#Buffer zone is labeled as "burn unit" "8888".
setwd(paste(comp, "Documents/GitHub/EglinAirForceBase/inputs", sep = ""))
unit_map <- matrix(scan("sef_bmap_1771x3491.txt", skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)

setwd(paste(ed, ":/FDM_2023_Simulation_Data/Step_01_FDM_Outputs/fuelbed_maps_step_1b_short_file_names", sep = ""))
fuelbed_map <- matrix(scan("f_050_001_05.asc", skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)

#Reset working directory
setwd(paste(ed, ":/FDM_2023_Simulation_Data/Step_02_Fuelbed_Derivative_Maps/maps", sep = ""))


#Create a list to hold min derivative values for each map
min_value <- vector()

#Create a vector to hold max derivative values for each map
max_value <- vector()

#Set up a nested loop to figure out total range of derivative values.
for(a in 1:length(rx_fire))
{
  for(b in 1:length(run_number))
  {
    
    #Create a list to hold file namnes
    filenames_in <- vector()
    
    #Create a list to hold maps
    maps_in <- list()
    
    #Add sim. year zero to map list
    maps_in[[1]] <- maps_orig
    
    for(c in 1:length(intervals))
    {
      #Generate file names of maps to import
      filenames_in[c] <- paste(prefix_in, rx_fire[a], "_", run_number[b], "_", intervals[c], ".asc", sep = "")
      
      #Import maps
      maps_in[[c+1]] <- matrix(scan(filenames_in[c],skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
    }
    
    #Identify the cell with the lowest change in derivative value.
    for(i in 1:length(maps_in))
    {
      min_value[length(min_value)+1] <- min(as.vector(maps_in[[i]][!unit_map %in% c(-9999, 8888) & !fuelbed_map %in% c(5099000, 6000000)]))
    }
    
    #Identify the cell with the highest change in derivative value.
    for(i in 1:length(maps_in))
      {
      max_value[length(max_value)+1] <- max(as.vector(maps_in[[i]][!unit_map %in% c(-9999, 8888) & !fuelbed_map %in% c(5099000, 6000000)]))
      }
    
    rm(maps_in)
    rm(filenames_in)
    print(paste("Identifying range:  Rx_fire", rx_fire[a], "Run", run_number[b], sep = " "))
  }
}

d_min <- round(min(min_value)-1,0)
d_max <- round(max(max_value)+1,0)


#Create a list to accespt outputs
derivative_data <- list()

#Generate tabular data
#Set up a nested loop to process all simulation maps (cannot import all of them at once, there are too many (400 maps).
for(a in 1:length(rx_fire))
{
  for(b in 1:length(run_number))
  {
    
    #Create a list to hold file namnes
    filenames_in <- vector()
    
    #Create a list to hold maps
    maps_in <- list()
    
    #Add sim. year zero to map list
    maps_in[[1]] <- maps_orig
    
    for(c in 1:length(intervals))
    {
      #Generate file names of maps to import
      filenames_in[c] <- paste(prefix_in, rx_fire[a], "_", run_number[b], "_", intervals[c], ".asc", sep = "")
      
      #Import maps
      maps_in[[c+1]] <- matrix(scan(filenames_in[c],skip = f.head@file@offset),ncol=f.head@ncols,byrow=T)
    }
    
    #################################################################################################
    #Set up matrix to hold fine fuel load values
    derivative_matrix <- matrix(nrow = length(maps_in), ncol = length(seq(d_min,d_max,1)))
    
    d_values <- d_min:d_max
    for(n in 1:length(maps_in))
    {
      for(o in 1:length(d_values))
      {
        derivative_matrix[n,o] <- length(maps_in[[n]][!unit_map %in% c(-9999, 8888) & !fuelbed_map %in% c(5099000, 6000000) & maps_in[[n]] > (d_values[o]-1) & maps_in[[n]] <= d_values[o]])
      }
    }
    colnames(derivative_matrix) <- d_values
    derivative_data[[1+length(derivative_data)]] <- derivative_matrix
    rm(maps_in)
    rm(filenames_in)
    print(paste("Generating Tabular Data:  Rx_fire", rx_fire[a], "Run", run_number[b], sep = " "))
  }
}


#Create a spreadsheet from list
dm <- matrix()
for(i in 1:length(derivative_data))
{
  if(i == 1)
  {
    dm <- derivative_data[[i]]
  } else
  {
    dm <- rbind(dm, derivative_data[[i]])
  }
}

df <- expand.grid(seq(0,50,5), 1:10, c(50,75,100,125))
ddf <- data.frame(rx_fire = df$Var3,
                   run_no = df$Var2,
                   sim_yr = df$Var1)

derivative_df <- cbind(ddf, dm)

setwd(paste(ed, ":/FDM_2023_Simulation_Data/Step_05_Derivative_Tables", sep = ""))
write.csv(derivative_df, file = paste("Derivative_table_", prefix_name, ".csv", sep = ""),
           row.names = FALSE)#

#####################################################################################################
#END