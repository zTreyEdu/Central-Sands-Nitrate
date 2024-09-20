#Project Title: Central Sands Nitrate Correlation - Romano Database and Land Use
#Project Description: evaluates the correlation between the nitrate levels from the romano database, and land use

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
#' Read in shapefiles for our cells containing nitrate data
#' Taken from the Romano Database
#' @returns a dataframe of polygon shapefiles with Nitrate information
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

scrDots <- function(dotCount) {
  if ((dotCount %% 1000) == 0 ) {
    cat(dotCount %/% 1000)
  }
  else if ((dotCount %% 100) == 0) {
    cat(".")
  }
  dotCount <- (dotCount + 1)
  return(dotCount)
}

getContributingPointsInfoForNitrateCells <- function(nitrateCells, timeFrameOfInterest, allModpathFlowlines, allModpathStartingPoints) {
  #Troubleshooting
  nitrateCells <- head(nitrateCells, 320)
  dotCount <- 0
  #End troubleshooting
  
  nitrateCells <- getFlowLinesInNitrateCell(nitrateCells, allModpathFlowlines)
  
  #ztrey left off here. I think the core of this is correct, so hooray!
  #However, when i run the loop, the system errors when the which() statement searches for 153424. My guess is foating point?
  #I see the rowIndex is stored as a long, so maybe convert stuff to ints?
  
  #get contributing point IDs from flowlines t01
  #for each cell in our nitrate cell data frame
  for (cell in 1:nrow(nitrateCells)) {
    dotCount <- scrDots(dotCount)
    flowlinesInCell <- nitrateCells[["IntersectingFlowlines"]][[cell]]
    
    #for each flowline in a cell
    particleRowIndexVector <- c()
    landUseVector <- c()
    
    for (flowlineIndex in flowlinesInCell) {
      partidloc <- allModpathFlowlines[["conversion_to_partidloc_"]][[flowlineIndex]]
      particleRowIndex <- which(allModpathStartingPoints$partidloc_ == partidloc)
      landUse <- allModpathStartingPoints[["CDL_2022_2"]][[particleRowIndex]]
      
      #troubleshooting
      #print(paste(cell, flowlineIndex, partidloc, particleRowIndex, landUse))
      particleRowIndexVector <- c( particleRowIndexVector,particleRowIndex)
      landUseVector <- c(landUseVector, landUse)
    }
    
    if (length(particleRowIndexVector) == 0) {
      particleRowIndexVector <- "no particles"
    }
    
    if (length(landUseVector) == 0) {
      landUseVector <- "no land use"
    }
    
    nitrateCells[["ContributingParticleRowIndex"]][[cell]] <- particleRowIndexVector
    nitrateCells[["ContributingParticleLandUse"]][[cell]] <- landUseVector
  }

  return(nitrateCells)  
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
  nitrateCellsWithContributingPointsInfo <- getContributingPointsInfoForNitrateCells(nitrateCells,timeFrameOfInterest,allModpathFlowlines,allModpathStartingPoints)
  
  # ----2.4 Find the land use for the contributing points----
  #ztrey - I think this is no longer needed
  #nitrateCellsWithLandUseMix <- getLandUseMix(allModpathStartingPoints, nitrateCellsWithContributingPoints, timeFrameOfInterest)
  
  # ----2.5 Find the estimated nitrogen impacts given the land use----
  #this loads in a data frame of joined up data into a regression analysis tool
  
  # ----2.6 Output to the user----
  #probably just a call to a function that print stuff and makes some plots
  
}

