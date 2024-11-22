#Project Title: Central Sands Nitrate Estimator
#Project Description: given a location, determine the estimated nitrate level


# TODO
#t03 - figure out what value buffer should be set to
#t04 - double check my RETURNs statements make sure i'm using correct with words like 'list' and 'dataframe' correctly
#t08 - add handling for null results
#t09 - confirm how I handle coordinate input and CRS syncing is correct
#t10 - is using CRS 3070 okay?
#t11.1 - review documentation
#t13 - look at changing function outputs to just add to the same data frame, rather than making a bunch of data frames

# -------------------Code begins here -----------------------

# ----0 Install packages, load libraries, and load global data----
#Packages
#install.packages('sf')
#install.packages('ggplot2')
#install.packages('raster')
#install.packages('dplyr')
#install.packages('devtools')
#install.packages('profvis')

#Libraries
library(sf)
library(ggplot2)
library(raster)
library(dplyr)
#library(devtools)
#library(profvis)


# ----1 Define Functions----
#' Get Value and Class_Name data from the Cropscape Raster tif
#' @returns a data frame with the Value and Class-Name Pair for the Cropscape Data set
getCropScapeClassNames <- function() {
  cropScapeRaster <- raster("//ad.wisc.edu/wgnhs/Projects/Central_Sands_Nitrate_Transport/GIS/CropScapeData/CDL_2022_20231127143930_554525100.tif")
  cropScapeLayers <- levels(cropScapeRaster)
  cropScapeClassNames <- cropScapeLayers[[1]] %>% #grab from the first layer
    dplyr::select(ID, CLASS_NAME)
  return(cropScapeClassNames)
}


#' Create an SF Point Object from a pair of coordinates
#' @param xCoord numeric, an x coordinate
#' @param yCoord numeric, a y coordinate
#' @returns an SF Point Object using CRS 3070
createSFPoint <- function(xCoord, yCoord) {
  point_geom <- st_point(c(xCoord, yCoord))
  sf_point <- st_sfc(point_geom, crs = 4326)
  sf_point <- st_transform(sf_point, crs = 3070)
  sf_point <- st_sf(geometry = sf_point)
  return(sf_point)
}

#' Gets the coordinates for which we'd like to estimate Nitrate levels.
#' Coordinates are currently hard-coded, but could be modified to allow for front-end user input
#' @returns coordinates stored as an SF Point Object
getCoordsOfInterest <- function() {
  xCoord <- -89.3551954
  yCoord <- 44.4980914
  coords <- createSFPoint(xCoord, yCoord)
  return(coords)
}

#' Gets the time frame we should be looking back
#' Time is currently hard-coded, but could be modified to allow for front-end user input
#' @returns a time in years
getTimeFrameOfInterest <- function() {
  timeInYears <- 100
  return(timeInYears)
}

#' User input for how big our buffer zone should be
#' Buffer size is currently hard-coded, but could be modified to allow for front-end user input
#' @returns a number for our buffer size
getBuffer <- function() {
  buffer = 100 #t03, Ben's was set to 100, so just copying his for now.
  return(buffer)
}

#' Creates a buffer zone for a set of coordinates
#' @param coordsOfInterest a set of coordinates for which to generate a buffer zone around
#' @param buffer the size of the buffer to generate
#' @returns a buffer zone as a polygon object
getCoordBufferZone <- function(coordsOfInterest, buffer) {
  coordBufferZone <- st_buffer(coordsOfInterest, dist = buffer)
  return(coordBufferZone)
}

#' Gets our MODPATH flow lines by reading them in from their shape files
#' Objects from this data set will b abbreviated as FLO objects
#' source_file and conversion_to_partiloc_ are added so we can correctly match up IDs later on.
#' 
#' Note: the fortran code used to generate the flow lines had to be run several times and could not directly create unique IDs, hence the need to subtract or add certain numbers
#' the specific numbers used (-2 and +79740) come from how the original ID generation code was written.
#' @returns a data frame that has our MODPATH flow lines, including a new column indicating which file a given row came from
getFloDataSet <- function() {
  floDataSet0 <- st_read(dsn = "//ad.wisc.edu/wgnhs/Projects/Central_Sands_Nitrate_Transport/R_Analysis/Data Sets/Particles_updated_June2024/1particle_top_pathlines_0.shp")
  floDataSet0$source_file <- "pathlines0"
  floDataSet0$conversion_to_partidloc_ <- (floDataSet0$particleid - 2)
  
  floDataSet1 <- st_read(dsn = "//ad.wisc.edu/wgnhs/Projects/Central_Sands_Nitrate_Transport/R_Analysis/Data Sets/Particles_updated_June2024/1particle_top_pathlines_1.shp")
  floDataSet1$source_file <- "pathlines1"
  floDataSet1$conversion_to_partidloc_  <- (floDataSet1$particleid + 79740)
  
  floDataSet <- rbind(floDataSet0, floDataSet1)
  
  #remove rows where the travel time is 0. These won't have start/end points, and are essentially bogus rows
  floDataSet <- floDataSet %>%
    filter(time != 0)
  
  return(floDataSet)
}
#' Gets our MODPATH starting points by reading them from their shape file
#' Objects from this data set will be abbreviated as STP objects
#' @returns a data frame with our MODPATH starting points
getStpDataSet <- function() {
  stpDataSet <- st_read(dsn = "//ad.wisc.edu/wgnhs/Projects/Central_Sands_Nitrate_Transport/R_Analysis/Data Sets/Particles_updated_June2024/1particle_data_top_startpt.shp")
  return(stpDataSet)
}

#' Finds the flow lines that intersect with our buffer zone
#' @param coordBufferZone a polygon object for the buffer zone in question 
#' @param floDataSet linestring objects, flow lines from our MODPATH model output
#' @returns a data frame of FLOs
getFLOsInBufferZone <- function(coordBufferZone, floDataSet) {
  intersections <- st_intersects(coordBufferZone, floDataSet)
  flosInBufferZone <- floDataSet[intersections[[1]], ]
  return(flosInBufferZone)
}

#' Finds the contributing points (contribSTPs) of our flow lines
#' @param flosInBufferZone a data frame of linestring objects, the flow lines that intersected our buffered region
#' @returns a data frame of particle IDs for the contributing points. Note: the IDs are just a list of IDs, not the actual SF Objects.
#'          The actual SF objects need to be retrieved from the stpDataSet later on
getContribSTPsFromFLOs <- function(flosInBufferZone) {
  contribSTPIDs <- data.frame(partidloc_ = flosInBufferZone$conversion_to_partidloc_)
  return(contribSTPIDs)
}

#' Get the starting points that contribute to our area of interest
#' @param coordsOfInterest a set of coordinates for which to generate a buffer zone around
#' @param buffer the size of the buffer to draw around our coordinates of interest
#' @param timeFrameOfInterest a time in years
#' @param floDataSet line string objects, flow lines from our MODPATH model output
#' @returns a list with the following structure:
#'          stpIDs: a list of STP IDs for our contributing zones
#'          floIDs: a list of FLO IDs for the flow lines that intersect the buffer zone of the selected area
#'          bufferZone: the polygon for our buffer zone
getContribSTPsForCoord <- function(coordsOfInterest, buffer, timeFrameOfInterest, floDataSet) {
  coordBufferZone <- getCoordBufferZone(coordsOfInterest, buffer)
  flosInBufferZone <- getFLOsInBufferZone(coordBufferZone,floDataSet)
  contribSTPIDs <- getContribSTPsFromFLOs(flosInBufferZone)
  contribSTPsForCoordReturnList <- list(stpIDs = contribSTPIDs, floIDs = flosInBufferZone, bufferZone = coordBufferZone)
  return(contribSTPsForCoordReturnList)
}

#' Display the x,y coordinates for a given set of particles in the contributing zone
#' Note: This function is not used in the main code, and is only intended to help with troubleshooting
#' @param stpIDs a data frame of particle IDs whose coordinates we want to display
#' @param stpDataSet a data frame of STPs from our MODPATH model
displayCoordsForSTPIDs <- function(stpDataSet, stpIDs) {
  foreignKey <- c("partidloc_" = "partidloc_")
  stpIDs <- stpDataSet %>%
    inner_join(contributingPoints, by = foreignKey) %>%
    dplyr::select(partidloc_, x, y) #using package::function notation as 'select' is a common name
  print(stpIDs)
}


#' Get the land use mix for our starting points
#' @param stpDataSet a data frame of STPs from our MODPATH model
#' @param stpIDs a data frame of STP IDs
#' @param timeFrameOfInterest a time in years
#' @returns a data frame of land cover
getLandCoverMix <- function(stpDataSet, stpIDs, timeFrameOfInterest) {
  foreignKey <- c("partidloc_" = "partidloc_")
  landCoverMix <- stpDataSet %>%
    inner_join(stpIDs, by = foreignKey) %>%
    dplyr::select(partidloc_, `CDL_2022_2`) #For now, just use the most recent land use; #using package::function notation as 'select' is a common name
  
  #Add in the Class Names for the land cover values so us humans can understand
  cropScapeClassNames <- getCropScapeClassNames()
  landCoverMix <- landCoverMix %>%
    left_join(cropScapeClassNames, by = c("CDL_2022_2" = "ID"))
  
  return(landCoverMix)
}

#' Summarize the contributing zones by landuse mix
#' @param landCoverMix a data frame with one row per STP ID and a column for land cover
#' @returns a data frame with one row per land cover, a column for the Count of particle IDs with that land cover, and a column with the relative frequency of that land use
getSummarizedLandCoverMix <- function(landCoverMix) {
  summarizedLandCoverMix <- landCoverMix %>%
    group_by(CLASS_NAME) %>%
    summarise(
      CDL_2022_Count = n(),
      CDL_2022_Relative = n() / nrow(landCoverMix)
    )
  return(summarizedLandCoverMix)
}

#' Calculates the estimated nitrate levels given a land cover mix
#' @param landCoverMix a data frame with one row per particle IDs and a column for land cover
#' @return ???
getEstimatedNitrateLevels <- function(landCoverMix) {
  # I think it'll eventually be an equation that does something like:
  #      Nitrate = (c1)Corn + (c2)Potato + (c3)Woodlands, etc. where each 'c' is a constant for how much land cover contributes to NO3.
  # We'd also like a way to do error bars. Which I *think* we'll just get for each term in our equation. And then...I think you just add the errors? I'll ask my Stats TAs
  # so i might need ways to hand:
  # -- a function to set up constants
  # -- additional handling for factoring in different years (it's currently just using 2022 data)
  # -- a function that runs the equation
  # -- output
  
  summarizedLandCovereMix <- getSummarizedLandCoverMix(landCoverMix)
  return(summarizedLandCovereMix)
}

#' Create a bar plot
#' @param estimatedNitrateLevels a data frame of the landuse mix
#' @returns a bar plot of nitrate data
createPlots <- function(estimatedNitrateLevels) {
  #bar plot
  barPlotTitle = "Relative Land Cover of Contributing Zones"
  barPlot <- ggplot(estimatedNitrateLevels, aes(x = as.factor(CLASS_NAME), y = CDL_2022_Relative)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Land Cover", y = "Percent", title = barPlotTitle) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent)
  return(barPlot)
}

#' Core logic for our nitrate estimator
#' @param coordsOfInterest the coordinates for which to estimate nitrate levels
#' @param timeFrameOfInterest the time frame over which we should look (in years)
#' @param buffer the radius of our buffer zone (in meters)
#' @param floDataSet the flow lines (FLOs) generated from our MODPATH model
#' @param stpDataSet the starting points (STPs) from our MODPATH model
#' @returns a list with the following structure:
#'            stpIDs: a data frame of contributing point IDs
#'            floIDs: a data frame of FLO IDs that intersect with our selected buffer zone
#'            landCover: Land Cover fraction of all of the contributing zones
#'            bufferZone: a polygon of our buffer zone
#'            Index X: Estimated nitrate level (Note: not yet added in)
runNitrateEstimator <- function(coordsOfInterest, timeFrameOfInterest, buffer, floDataSet, stpDataSet) {
  #Find contributing FLOs and STPs for the coordinates of interest
  contribSTPsForCoordReturnList <- getContribSTPsForCoord(coordsOfInterest, buffer, timeFrameOfInterest, floDataSet)
  contribSTPIDs <- contribSTPsForCoordReturnList$stpIDs
  contribFLOIDs <- contribSTPsForCoordReturnList$floIDs
  bufferZone <- contribSTPsForCoordReturnList$bufferZone
 
  # Find the land cover for the contributing points
  landCoverMix <- getLandCoverMix(stpDataSet, contribSTPIDs, timeFrameOfInterest)
  
  # Find the estimated nitrogen impacts given the land use
  estimatedNitrateLevels <- getEstimatedNitrateLevels(landCoverMix)
  
  # Organize and return data
  nitrateEstimatorReturnList <- list(stpIDs = contribSTPIDs, floIDs = contribFLOIDs,landCover = estimatedNitrateLevels, bufferZone = bufferZone)
  return(nitrateEstimatorReturnList)
}

# ----2 Main Tag----
#' Main callable tag. Run this to estimate nitrate levels for a given set of coordinates
#' @returns estimated nitrate levels for a given set of coordinates, as well as some plots
mainNitrateEstimator <- function() {
  # ----2.1 User Input----
  coordsOfInterest <- getCoordsOfInterest()
  timeFrameOfInterest <- getTimeFrameOfInterest()
  buffer <- getBuffer()
  
  # ----2.2 Read in data----
  floDataSet <- getFloDataSet()
  stpDataSet <- getStpDataSet()
  
  # ----2.3 Find Nitrate Estimates----
  nitrateEstimatorReturnList <- runNitrateEstimator(coordsOfInterest, timeFrameOfInterest, buffer, floDataSet, stpDataSet)
  plots <- createPlots(nitrateEstimatorReturnList$landCover)
  print(plots)
}

