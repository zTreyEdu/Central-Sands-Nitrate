#Purpose: hold global info for the Interactive Map

#Libraries
library(sf)
library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)

source("//ad.wisc.edu/wgnhs/Projects/Central_Sands_Nitrate_Transport/R_Analysis/Central Sands Nitrate Estimator.R")

#Define some global variable
coordsOfInterest <- getCoordsOfInterest()
timeFrameOfInterest <- getTimeFrameOfInterest()
buffer <- getBuffer()
floDataSet <- getFloDataSet()
allModpathStartingPoints <- getAllModpathStartingPoints()


#Draw a boundary line for our pathline model
pathLineBoundary <- st_read(dsn = "//ad.wisc.edu/wgnhs/Projects/Central_Sands_Nitrate_Transport/R_Analysis/Misc_Shapefiles/Pathline Boundary.shp")
pathLineBoundary <- st_transform(pathLineBoundary, crs = 4326) %>%
  mutate(fill_color = "#1C00ff00")

#Define some functions

#' Let us know if the point the user selected is within our boundary
#' @param pathLineBoundary a polygon shape file of our region
#' @param marker a data frame of the longitude and latitude of the map marker
#' @returns a row of data if the user selelected something inside our region; returns 0 rows if they are outside the bounds
getRegionData <- function(pathLineBoundary, marker) {
  removeNotification(id = "region_error", session = getDefaultReactiveDomain())
  
  #set up our data frame
  dat <- data.frame(Longitude = marker$lng,
                    Latitude = marker$lat,
                    names = c("Point"))
  dat <- st_as_sf(dat, coords = c("Longitude",
                                  "Latitude"))
  st_crs(dat) <- st_crs(pathLineBoundary)
  
  #Do the heavy lifting to determine if our selected point is inside our shapefile
  return(as.data.frame(pathLineBoundary)[which(sapply(st_intersects(pathLineBoundary,dat), function(z) if (length(z)==0) NA_integer_ else z[1]) == 1), ])
}

#'Given a longitude and latitude, generate a plot of nitrogen estimate info
#' @param longitude a longitude
#' @param latitude a latitude
#' @returns a bar plot of nitrate data
generateNitrateEstimates <- function(longitude, latitude) {
  selectedCoords <- createSFPoint(longitude, latitude)
  estimatedNitrateLevels <- runNitrateEstimator(selectedCoords, timeFrameOfInterest, buffer, floDataSet, allModpathStartingPoints)
  nitrateBarPlot <- createPlots(estimatedNitrateLevels)
  return(nitrateBarPlot)
}
  
  
  