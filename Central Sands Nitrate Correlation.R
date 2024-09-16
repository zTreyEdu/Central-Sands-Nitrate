#Project Title: Central Sands Nitrate Correlation - Romano Database and Land Use
#Project Description: evaluates the correlation between the nitrate levels from the romano database, and land use


#TODO Next
#I'm workign on getContributingPointsForNitrateCells and it's breaking my brain
#I got a bit stuck with how to use R functions to essentially do some nested for loops for object lookup
# that said, i think I should use a for loop to iterate over the vectors stored in nitrateCells$IntersectingFlowLines.
# however, the last round of ChatGPT made sapply seem straightforward enough. I think the trick here is not to overthink. This problem has been solved before.

#TODO
#t01 - add ability to pull in contributing points
#t02 - look at changing function outputs to just add to the same data frame, rather than making a bunch of data frames

# -------------------Code begins here -----------------------

# ----0 Install Packages and Libraries----
#install.packages('sf')
#install.packages('ggplot2')
#install.packages('raster')
#install.packages('dplyr')
#install.packages('devtools')
#install.packages('profvis')

library(sf)
library(ggplot2)
library(raster)
library(dplyr)
#library(devtools)
#library(profvis)

source("Central Sands Nitrate Estimator.R")

# ----1 Define Functions----
getNitrateCells <- function() {
  nitrateCells <- st_read(dsn = "U:/Trey Coury/DS002-dataset01/CSGCC_Nitrate_Neonicotinoids.gdb", layer = "Nitrate_stats_per_section_Non_PWS_2017_2022")
  nitrateCells <- st_transform(nitrateCells, crs = 3070)
  return(nitrateCells)
}

getFlowLinesInNitrateCell <- function(nitrateCells, allModpathFlowlines) {
  intersections <- st_intersects(nitrateCells, allModpathFlowlines)
  nitrateCells$IntersectingFlowlines <- intersections
  return(nitrateCells)
}

getContributingPointsForNitrateCells <- function(nitrateCells, timeFrameOfInterest, allModpathFlowlines) {
  nitrateCells <- getFlowLinesInNitrateCell(nitrateCells, allModpathFlowlines)
  #get contributing point IDs from flowlines t01
  nitrateCells <- mutate()

  
}

# ----2 Main Callable Tag----

mainNitrateCorrelator <- function(){
  # ----2.1 User Input----
  timeFrameOfInterest <- getTimeFrameOfInterest()
  
  # ----2.2 Read in datafiles----
  allModpathFlowlines <- getAllModpathFlowLines()
  allModpathStartingPoints <- st_read(dsn = "P:/Central_Sands_Nitrate_Transport/GIS/ModelOutput/Particles_updated_June2024/1particle_data_top_startpt.shp")
  nitrateCells <- getNitrateCells()
  
  # ----2.3 Find contributing points for our Nitrate Cells----
  nitrateCellsWithContributingPoints <- getContributingPointsForNitrateCells(nitrateCells,timeFrameOfInterest,allModpathFlowlines)
  
    # ----2.4 Find the land use for the contributing points----
  nitrateCellsWithLandUseMix <- getLandUseMix(allModpathStartingPoints, nitrateCellsWithContributingPoints, timeFrameOfInterest)
  
  # ----2.5 Find the estimated nitrogen impacts given the land use----
  #this loads in a data frame of joined up data into a regression analysis tool
  
  # ----2.6 Output to the user----
  #probably just a call to a function that print stuff and makes some plots
  
}

