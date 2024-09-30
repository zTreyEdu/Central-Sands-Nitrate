#Purpose: hold global info for the Interactive Map

#Libraries
library(sf)
library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)

source("U:/Trey Coury/Central Sands Nitrate/Central Sands Nitrate Estimator.R")

#Define some global variable
coordsOfInterest <- getCoordsOfInterest()
timeFrameOfInterest <- getTimeFrameOfInterest()
buffer <- getBuffer()
allModpathFlowlines <- getAllModpathFlowLines()
allModpathStartingPoints <- st_read(dsn = "P:/Central_Sands_Nitrate_Transport/GIS/ModelOutput/Particles_updated_June2024/1particle_data_top_startpt.shp")


#Draw a boundary line for our pathline model
pathLineBoundary <- st_read(dsn = "U:/Trey Coury/ArcGIS/MyProject/Pathline Boundary.shp")
pathLineBoundary <- st_transform(pathLineBoundary, crs = 4326) %>%
  mutate(fill_color = "#1C00ff00")

#Define some functions
region_data <- function(pathLineBoundary, markers) {
  removeNotification(id = "region_error", session = getDefaultReactiveDomain())
  
  dat <- data.frame(Longitude = markers$lng,
                    Latitude = markers$lat,
                    names = c("Point"))
  
  dat <- st_as_sf(dat, coords = c("Longitude",
                                  "Latitude"))
  
  st_crs(dat) <- st_crs(pathLineBoundary)
  return(as.data.frame(pathLineBoundary)[which(sapply(st_intersects(pathLineBoundary,dat), function(z) if (length(z)==0) NA_integer_ else z[1]) == 1), ])
}

generateNitrateEstimates <- function(longitude, latitude) {
  selectedCoords <- createSFPoint(longitude, latitude)
  estimatedNitrateLevels <- runNitrateEstimator(selectedCoords, timeFrameOfInterest, buffer, allModpathFlowlines, allModpathStartingPoints)
  nitrateBarPlot <- createPlots(estimatedNitrateLevels)
  return(nitrateBarPlot)
}
  
  
  