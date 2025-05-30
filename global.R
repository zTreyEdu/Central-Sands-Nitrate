#Purpose: hold global info for the Interactive Map

#Install packages
#install.packages("sf")
#install.packages("shiny")
#install.packages("leaflet")
#install.packages('leaflet.extras2')
#install.packages("dplyer")
#install.packages("shinydashboard")
#install.packages("shinycssloaders")

# 0 Libraries----
library(sf)
library(shiny)
library(leaflet)
library(leaflet.extras2)
library(dplyr)
library(shinydashboard)
library(shinycssloaders)

source("Central Sands Nitrate Estimator.R")

# 1 Define Functions----
#' Let us know if the point the user selected is within our boundary
#' @param pathLineBoundary a polygon shape file of our region
#' @param marker a data frame of the longitude and latitude of the map marker
#' @returns a row of data if the user selected something inside our region; returns 0 rows if they are outside the bounds
getRegionData <- function(pathLineBoundary, marker) {
  removeNotification(id = "region_error", session = getDefaultReactiveDomain())
  
  #set up our data frame
  dat <- data.frame(Longitude = marker$lng,
                    Latitude = marker$lat,
                    names = c("Point"))
  dat <- st_as_sf(dat, coords = c("Longitude",
                                  "Latitude"))
  st_crs(dat) <- st_crs(pathLineBoundary)
  
  #Do the heavy lifting to determine if our selected point is inside our shape file
  return(as.data.frame(pathLineBoundary)[which(sapply(st_intersects(pathLineBoundary,dat), function(z) if (length(z)==0) NA_integer_ else z[1]) == 1), ])
}

#'Given a longitude and latitude, generate a info about up gradient land cover
#' @param longitude a longitude
#' @param latitude a latitude
#' @returns a list of return values. View runNitrateEstimator and createPlots for list structure
generateNitrateEstimates <- function(longitude, latitude) {
  selectedCoords <- createSFPoint(longitude, latitude)
  landCoverCode <- getLandCoverCode()
  nitrateEstimatorReturnList <- runNitrateEstimator(selectedCoords, timeFrameOfInterest, buffer, floDataSet, stpDataSet, landCoverCode)
  nitrateEstimatorReturnList <- createPlots(nitrateEstimatorReturnList, landCoverCode)
  return(nitrateEstimatorReturnList)
}

#' Given a data frame of STP IDs (the IDs for starting points), look up their SF data, and return their coordinates
#' @param stpIDs a data frame of STP IDs
#' @returns a matrix of coordinates for our STPs
getSTPCoords <- function(stpIDs) {
  #stpIDs is just a list of IDs, not SF objects
  #We get the actual SF Object from our stpDataSet
  sfSTPIDs <- stpDataSet %>%
    filter(partidloc_ %in% stpIDs$partidloc_)
  sfSTPIDs <- st_transform(sfSTPIDs, crs = 4326)
  
  #Now that we have the sf objects, get their coordinates
  stpCoords <- st_coordinates(sfSTPIDs) %>%
    as.data.frame() %>%
    rename(lng = X, lat = Y) #rename columns for the addCircleMarkers call we'll make later on
  return(stpCoords)
}

#'Given a data frame of FLO IDs, projects them to CRS 4326
#' @param floIDs a data frame of FLO objects
#' @returns a data frame of FLO IDs projected to CRS 4326
getFLOProjection <- function(floIDs) {
  floIDs <- st_transform(floIDs, crs = 4326)
  return(floIDs)
}

# 2 Feature switches----
#set these to 1 to enable the functionality; set to 0 to disable
displayContribSTPs <- 1 #show circle markers for the starting points of each contributing zone
displayContribFLOs <- 1 #show the MODPATH lines or each of the flow paths that intersect the selected buffer region 
displayBufferZone <- 1 #show an outline of the buffer region we're using to select our flow paths

# 3 Define some global variable----
defaultLng <- -89.518247 #center around Plainsfield, WI
defaultLat <- 44.210243
coordsOfInterest <- getCoordsOfInterest()
timeFrameOfInterest <- getTimeFrameOfInterest()
buffer <- getBuffer()
floDataSet <- getFloDataSet()
stpDataSet <- getStpDataSet()

# 4 Create projected FLOs to display on our map----
floDisplaySet <- slice_sample(floDataSet, n = 1500)
floDisplaySet <- getFLOProjection(floDisplaySet)

# 5 Create a boundary line for our MODPATH model (polygon data)----
pathLineBoundary <- st_read(dsn = "Misc_Shapefiles/prelim_ff_model_bounds_proposed.shp")
pathLineBoundary <- st_transform(pathLineBoundary, crs = 4326) %>%
  mutate(fill_color = "#1C00ff00")

# 6 read in wells (point data)----
pumpingWells <- st_read(dsn = "Misc_Shapefiles/pumping_well_pts.shp")
pumpingWells <- st_transform(pumpingWells, crs = 4326)
  
  
  