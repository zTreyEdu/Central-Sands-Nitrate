#Project Title: Central Sands Nitrate Estimator
#Project Description: given a location, determine the estimated nitrate level


# TODO
#t03 - figure out what value buffer should be set to
#t04 - double check my RETURNs statements make sure i'm using correct with words like 'list' and 'dataframe' correctly
#t05 - as of 9 Sept, this ~7 seconds to run. Per profvis, the file reading is 5 seconds of this. Perhaps there's a way to query a database, rather than just loading it in? I could look for performance gains, elsewhere Which might mean i actually need to learn how some of these APIs work. Alas.
#t06 - update getLandUseMix to accommodate different times if needed. I'm still not certain on the logic, but I think it's either a loop to aggregate stuff, or just some subtraction for "get the land use from X years ago"
#t08 - add handling for null results
#t09 - confirm how I handle coordinate input and CRS syncing is correct
#t10 - is using CRS 3070 okay?
#t11 - re-do documentation for createPlots
#t11.1 - review documentation
#t12 - create function to output the coordinates of the contributing zone
#t13 - look at changing function outputs to just add to the same data frame, rather than making a bunch of data frames

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

# ----1 Define Functions----
#' Create an SF Point Object from a pair of coordinates
#'
#' @param xCoord numeric, an x coordinate
#' @param yCoord numeric, a y coordinate
#' @returns an SF Point Object using CRS 3070
createSFPoint <- function(xCoord, yCoord) {
  point_geom <- st_point(c(xCoord, yCoord))
  sf_point <- st_sfc(point_geom, crs = 4326) #t00 create from long,lat coords, then switch to CRS 3070
  sf_point <- st_transform(sf_point, crs = 3070)
  sf_point <- st_sf(geometry = sf_point)
  return(sf_point)
}

#' User input for the coordinates for which we'd like to estimate Nitrate levels.
#' Coordinates are currently hard-coded, but could be modified to allow for user-input
#' @returns coordinates stored as an SF Point Object
getCoordsOfInterest <- function() {
  #89.3551954°W 44.4980914°N
  xCoord <- -89.3551954
  yCoord <- 44.4980914
  coords <- createSFPoint(xCoord, yCoord)
  return(coords)
}

#' User input for the timeframe we should be looking back
#' Time is currently hard-coded, but could be modified to allow for user-input
#' @returns a time in years
getTimeFrameOfInterest <- function() {
  timeInYears <- 100
  return(timeInYears)
}

#' User input for how big our buffer zone should be
#' Buffer size is currently hard-coded, but could be modified to allow for user-input
#' @returns a number for our buffer size
getBuffer <- function() {
  buffer = 100 #t03, Ben's was set to 100, so just copying his for now.
  return(buffer)
}

#' Creates a buffer zone for a set of coordinates
#' @param coordsOfInterest a set of coordinates for which to generate a buffer zone around
#' @param buffer number, the size of the buffer to generate
#' @returns a buffer zone as a polygon object
getCoordBufferZone <- function(coordsOfInterest, buffer) {
  coordBufferZone <- st_buffer(coordsOfInterest, dist = buffer)
  return(coordBufferZone)
}

#' Gets our modpath flowlines by reading them in from their shapefiles
#' Objects from this data set will b abbreviated as FLO objects
#' source_file and conversion_to_partiloc_ are added so we can correctly match up IDs later on.
#' (the fortran code used to generate the flowlines had to be run several times and could not directly create unique IDs, hence the conversions)
#' (the specific numbers used have are a consequence of how the original ID genreation code was written)
#' @returns a data frame that has our modpath flowlines, including a new column indicating which file a given row came from
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
#' Gets our modpath starting points by reading them from their shapefile
#' Objects from this data set will be abbreviated as STP objects
#' @returns a data frame with our modpath starting points
getStpDataSet <- function() {
  stpDataSet <- st_read(dsn = "//ad.wisc.edu/wgnhs/Projects/Central_Sands_Nitrate_Transport/R_Analysis/Data Sets/Particles_updated_June2024/1particle_data_top_startpt.shp")
  return(stpDataSet)
}

#' Finds the flow lines that intersect with our buffer zone
#' @param coordBufferZone a polygon object for the buffer zone in question 
#' @param floDataSet linestring objects, flowlines from our modpath model output
#' @returns a data frame of flowines
getFlowLinesInBufferZone <- function(coordBufferZone, floDataSet) {
  intersections <- st_intersects(coordBufferZone, floDataSet)
  flowLinesInBufferZone <- floDataSet[intersections[[1]], ]
  return(flowLinesInBufferZone)
}

#' Finds the contributing points of our flow lines
#' @param flowLinesInBufferZone a dataframe of linestring objects, the flowlines that intersected our buffered region
#' @returns a dataframe of particle IDs for the contributing points
getContributingPointIDsFromFlowlines <- function(flowLinesInBufferZone) {
  contributingPoints <- data.frame(partidloc_ = flowLinesInBufferZone$conversion_to_partidloc_)
  return(contributingPoints)
}

#' Get the starting points that contribute Nitrate to our area of interest
#' @param coordsOfInterest a set of coordinates for which to generate a buffer zone around
#' @param buffer number, the size of the buffer to generate
#' @param timeFrameOfInterest a time in years
#' @param floDataSet line string objects, flow lines from our modpath model output
#' @returns a data frame of particle IDs
getContributingPointsForCoord <- function(coordsOfInterest, buffer, timeFrameOfInterest, floDataSet) {
  coordBufferZone <- getCoordBufferZone(coordsOfInterest, buffer)
  flowLinesInBufferZone <- getFlowLinesInBufferZone(coordBufferZone,floDataSet)
  contributingPoints <- getContributingPointIDsFromFlowlines(flowLinesInBufferZone)
  return(contributingPoints)
}

#' Display the x,y coordinates for a given set of particles in the contributing zone
#' @param contributingPoints a dataframe of particle IDs whose coordinates we want to display
#' @param stpDataSet the list of starting points from our modpath model
displayCoordsForContribPoints <- function(stpDataSet, contributingPoints) {
  foreignKey <- c("partidloc_" = "partidloc_")
  coordsForContribPoints <- stpDataSet %>%
    inner_join(contributingPoints, by = foreignKey) %>%
    dplyr::select(partidloc_, x, y) #using package::function notation as 'select' is a common name
  print(coordsForContribPoints) #t12 - Rather than grabbing columns x and y, i think there's somethign special i need to do to grab the coords from the point objects. Because I think the projection won't line up otherwise
}


#' Get the land use mix for our starting points
#' @param contributingPoints a dataframe of particle IDs
#' @param stpDataSet the list of starting points from our modpath model
#' @param timeFrameOfInterest a time in years
#' @returns a data frame of land uses
getLandUseMix <- function(stpDataSet, contributingPoints, timeFrameOfInterest) {
  foreignKey <- c("partidloc_" = "partidloc_")
  landUseMix <- stpDataSet %>%
    inner_join(contributingPoints, by = foreignKey) %>%
    dplyr::select(partidloc_, `CDL_2022_2`) #For now, just use the most recent land use; #using package::function notation as 'select' is a common name
  return(landUseMix)
}

#' Summarize the contributing zones by landuse mix
#' @param landUseMix a dataframe with one row per particle IDs and a column for land use
#' @returns a data frame with one row per land use, a column for the Count of particle IDs with that land use, and a column with the relative frequency of that land use
getSummarizedLandUseMix <- function(landUseMix) {
  summarizedLandUseMix <- landUseMix %>%
    group_by(CDL_2022_2) %>%
    summarise(
      CDL_2022_Count = n(),
      CDL_2022_Relative = n() / nrow(landUseMix)
    )
  return(summarizedLandUseMix)
}

#' Calculates the estimated nitrate levels given a landuse mix
#' @param landUseMix a dataframe with one row per particle IDs and a column for land use
#' @return ???
getEstimatedNitrateLevels <- function(landUseMix) {
  # I think it'll eventually be an equation that does something like:
  #      Nitrate = (c1)Corn + (c2)Potato + (c3)Woodlands, etc. where each 'c' is a contstant for how much land use contributes to NO3.
  # We'd also like a way to do error bars. Which I *think* we'll just get for each term in our equation. And then...I think you just add the errors? I'll ask my Stats TAs
  # so i might need ways to hand:
  # -- a function to set up constants
  # -- additional handling for factoring in different years (it's currently just using 2022 data)
  # -- a function that runs the equation
  # -- output
  
  summarizedLandUseMix <- getSummarizedLandUseMix(landUseMix)
  return(summarizedLandUseMix)
}

#' Create some plots for our users
#' @note: function documented on 9 September 2024 when it was like pre-alpha version. Update later as appropriate. t11
#' @param estimatedNitrateLevels a data frame of the landuse mix
#' @returns a bar plot of nitrate data
createPlots <- function(estimatedNitrateLevels) {
  #bar plot
  barPlotTitle = "Relative Land Use of Contributing Zones"
  barPlot <- ggplot(estimatedNitrateLevels, aes(x = as.factor(CDL_2022_2), y = CDL_2022_Relative)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Land Use", y = "Percent", title = barPlotTitle) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent)
  ggsave("Relative Land Use of Contributing Zones.png", barPlot)
  return(barPlot)
}

#' Core logic for our nitrate estimator
#' @param coordsOfInterest the coordinates for which to estimate nitrate levels
#' @param timeFrameOfInterest the time frame over which we should look
#' @param buffer the radius of our buffer zone (in meters)
#' @param floDataSet the flowlines generated from our MODPATH model
#' @param stpDataSet the starting points from our MODPATH model
#' @returns a list with the following structure:
#'            stpIDs: a data frame of contributing point IDs
#'            landCover: Land Cover fraction of all of the contributing zones
#'            Index 3: Estimated nitrate level (Note: not yet added in)
runNitrateEstimator <- function(coordsOfInterest, timeFrameOfInterest, buffer, floDataSet, stpDataSet) {
  contributingPoints <- getContributingPointsForCoord(coordsOfInterest, buffer, timeFrameOfInterest, floDataSet)
  print(contributingPoints)
  displayCoordsForContribPoints(stpDataSet, contributingPoints)
  
  # ----2.4 Find the land use for the contributing points----
  landUseMix <- getLandUseMix(stpDataSet, contributingPoints, timeFrameOfInterest)
  print(landUseMix)
  
  # ----2.5 Find the estimated nitrogen impacts given the land use----
  estimatedNitrateLevels <- getEstimatedNitrateLevels(landUseMix)
  print(estimatedNitrateLevels)
  
  # ----2.6 Output to the user----
  returnList <- list(stpIDs = contributingPoints, landCover = estimatedNitrateLevels)
  
  return(returnList)
}

# ----2 Main Tag----
#' Main callable tag. Run this to estimate nitrate levels for a given set of coordinates
#' @returns estimated nitrate levels for a given set of coordinates, as well as some plots
mainNitrateEstimator <- function() {
  # ----2.1 User Input----
  coordsOfInterest <- getCoordsOfInterest()
  timeFrameOfInterest <- getTimeFrameOfInterest()
  buffer <- getBuffer()
  
  # ----2.2 Read in datafiles----
  floDataSet <- getFloDataSet()
  stpDataSet <- getStpDataSet()
  
  # ----2.3 Find Nitrate Estimates----
  nitrateEstimatorReturnList <- runNitrateEstimator(coordsOfInterest, timeFrameOfInterest, buffer, floDataSet, stpDataSet)
  plots <- createPlots(nitrateEstimatorReturnList$landCover)
  print(plots)
  print("placeholder to know we're done")
}

