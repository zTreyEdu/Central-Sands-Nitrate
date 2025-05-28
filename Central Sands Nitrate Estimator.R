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
#15 - add proper prediction equation and clean up getEstimatedNitrateLevels

# -------------------Code begins here -----------------------

# ----0 Install packages, load libraries, and load global data----
#Packages
#install.packages('sf')
#install.packages('ggplot2')
#install.packages('raster')
#install.packages('dplyr')
#install.packages('devtools')
#install.packages('profvis')
#install.packages("forcats")
#install.pakcages("tidyr")

#Libraries
library(sf)
library(ggplot2)
library(raster)
library(dplyr)
library(forcats)
library(lwgeom)
library(tidyr)
#library(devtools)
#library(profvis)


# ----1 Define Functions----

#' Calculates the estimated nitrate levels given a WiscLand land cover mix
#' @param landCoverMix a data frame with one row per particle IDs and a column for land cover
#' @returns a list with the following structure:
#'            summarizedLandCoverMix - a data frame with one row per land cover and columns for the amount of each land cover
#'            no3Prediction - a data frame with the no3 prediction. It has the fit value, as well as the prediction interval lower and upper bound
getEstimatedNitrateLevelsWiscLand <- function(landCoverMix) {
  
  # summarizedLandCoverMix <- landCoverMix %>%
  #   group_by(cls_desc_3) %>%
  #   summarise(
  #     LAND_COVER_COUNT = n(),
  #     LAND_COVER_RELATIVE = (n() / nrow(landCoverMix))
  #   )
  
  summarizedLandCoverMix <- landCoverMix %>%
    count(cls_desc_1, cls_desc_3, name = 'LAND_COVER_COUNT')
  
  #Note: as of this writing (28 May 2025), we did not yet have a linear model to use for WiscLand. Therefore, we'll use these placeholder values.
  #See getEstimatedNitrateLevelsCropscape for an example of how to import a linear model.
  no3Prediction <- data.frame(fit = 50, lwr = 0, upr = 100)
  
  #Combine into our return list
  estimatedNitrateLevelsReturnList <- list(summarizedLandCoverMix = summarizedLandCoverMix, no3Prediction = no3Prediction)
  
  return(estimatedNitrateLevelsReturnList)
}

#' Wrapper function to get the estimated Nitrate Level depending on the land cover data set
#' @param landCoverMix a data frame with one row per particle IDs and a column for land cover
#' @param landCoverCode the code for the land cover data set to use
#' @returns a list with the following structure:
#'            summarizedLandCoverMix - a data frame with one row per land cover and columns for the amount of each land cover
#'            no3Prediction - a data frame with the no3 prediction. It has the fit value, as well as the prediction interval lower and upper bound
getEstimatedNitrateLevelsWrapper <- function(landCoverMix,landCoverCode) {
  if (landCoverCode == 1) {
    estimatedNitrateLevelsReturnList <- getEstimatedNitrateLevelsCropScape(landCoverMix)
  } else if (landCoverCode == 2) {
    estimatedNitrateLevelsReturnList <- getEstimatedNitrateLevelsWiscLand(landCoverMix)
  } else{
    estimatedNitrateLevelsReturnList <- getEstimatedNitrateLevelsCropScape(landCoverMix) #we'll default to CropScape. CropScape was chosen arbitrarily.
  }
  
  return(estimatedNitrateLevelsReturnList)
}

#' Get the WiscLand land use mix for our starting points
#' @param stpDataSet a data frame of STPs from our MODPATH model
#' @param stpIDs a data frame of STP IDs
#' @param timeFrameOfInterest a time in years
#' @returns a data frame of land cover
getLandCoverMixWiscLand <- function(stpDataSet, stpIDs, timeFrameOfInterest){
  foreignKey <- c("partidloc_" = "partidloc_")
  landCoverMix <- stpDataSet %>%
    inner_join(stpIDs, by = foreignKey) %>%
    dplyr::select(partidloc_,
                  cls_lvl_1, cls_desc_1,
                  cls_lvl_2, cls_desc_2,
                  cls_lvl_3, cls_desc_3,
                  cls_lvl_4, cls_desc_4) #extract all levels so they can be used later if desired
  
  return(landCoverMix)
}

#' Wrapper function to get land cover mix depending on the land cover data set
#' @param stpDataSet a data frame of STPs from our MODPATH model
#' @param stpIDs a data frame of STP IDs
#' @param timeFrameOfInterest a time in years
#' @param landCoverCode the code for the land cover data set to use
#' @returns a data frame of land cover
getLandCoverMixWrapper <- function(stpDataSet, contribSTPIDs, timeFrameOfInterest,landCoverCode) {
  if(landCoverCode == 1) {
    landCoverMix <- getLandCoverMixCropScape(stpDataSet, contribSTPIDs, timeFrameOfInterest)
  } else if (landCoverCode == 2) {
    landCoverMix <- getLandCoverMixWiscLand(stpDataSet, contribSTPIDs, timeFrameOfInterest)
  } else{
    landCoverMix <- getLandCoverMixCropScape(stpDataSet, contribSTPIDs, timeFrameOfInterest) #we'll default to CropScape. CropScape was chosen arbitrarily.
  }
  
  return(landCoverMix)
}

#' User input for the land cover data set to use. This is currently hard-coded, but could be updated to allow for prompting a user.
#' 
#' Use the following codes:
#'    1 - CropScape
#'    2 - WiscLand 2, Level 3
#'    
#' @returns a number corresponding to the land cover data set to use
getLandCoverCode <- function() {
  landCoverCode <- 2
  return(landCoverCode)
}


#' Get Value and Class_Name data from the Cropscape Raster tif
#' @returns a data frame with the Value and Class-Name Pair for the Cropscape Data set
getCropScapeClassNames <- function() {
  cropScapeRaster <- raster("Data Sets/CropScape/CDL_2022_20231127143930_554525100.tif")
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
  xCoord <- -89.6168518
  yCoord <- 44.1309709
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
  floDataSet0 <- st_read(dsn = "Data Sets/Particles_Pathlines_May2025/1particle_top_pathlines_0.shp")
  floDataSet0$source_file <- "pathlines0"
  floDataSet0$conversion_to_partidloc_ <- (floDataSet0$particleid - 2)
  
  floDataSet1 <- st_read(dsn = "Data Sets/Particles_Pathlines_May2025/1particle_top_pathlines_1.shp")
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
  #stpDataSet <- st_read(dsn = "Data Sets/Particles_updated_June2024/1particle_data_top_startpt.shp") #ztrey - commented out for testing
  stpDataSet <- st_read(dsn = "Data Sets/Particles_Pathlines_May2025/startpoints_with_wiscland.shp")
  return(stpDataSet)
}

#' given a data frame of SFs, a row, and a column, return a numeric value from a cell
#' @param dataFrame a dataframe of SF objects
#' @param rowIx the row index to look up
#' @param columnName the name of the column to look up
#' @returns the value as a numeric data type
getNumValueFromSFDF <- function(dataFrame, rowIx, columnName) {
  cellValue <- dataFrame[rowIx, columnName]
  cellValueNumeric <- cellValue %>%
    st_drop_geometry() %>%
    as.numeric()
  return(cellValueNumeric)
}

#' Given a data from of split FLOs, find the length and travel time of each segment
#' @param floSet an FLO data frame
#' @returns floSet with a column for FLO segment length and a column for segment travel time
getFLOSegmentLengths <- function(floSet) {
  floSet <- floSet %>%
    mutate(segment_length = st_length(geometry)) %>% #returns length with units stored as an attribute, so split those out
    mutate(segment_length = as.numeric(segment_length)) %>%
    mutate(segment_time = ((segment_length / total_length) * time))
  return(floSet)
}

#' Given a data frame of FLOs, find the length of each line and split the units out into a separate column
#' @param floSet an FLO data frame
#' @returns floSet with a column for FLO length and a column for the units
getFLOTotalLengths <- function(floSet) {
  #Create our total length column
  floSet <- floSet %>%
    mutate(total_length = st_length(geometry))
  
  #Split it into two columns
  floSet <- floSet %>%
    mutate(length_units = as.character(units(floSet$total_length))) %>%
    mutate(total_length = as.numeric(total_length))
  
  return(floSet)
}

#' Get the time it takes for the FLOs to reach the middle of the selected region
#' @param floSet a data frame of FLOs
#' @param coordBufferZone the buffer zone of our selected point
#' @returns the floSet with a column for how long it takes the ground water to reach the middle of the selected region
getFLOTimes <- function(floSet, coordBufferZone) {
  #Get the total length of the FLOs
  floSet <- getFLOTotalLengths(floSet)

  #Split the FLOs into 3 sections based on where they intersect the buffer zone: 1) before intersecting, 2) inside the buffer, 3) after the buffer
  #Stictly speaking, it's possible something just touched the buffer without going inside (e.g., a tangent line), in which case we'll only have it split into 2.
  #This is a rare scenario, but we'll handle it regardless
  floSplitSet <- st_split(floSet, coordBufferZone)
  
  #Extract the output of st_split into something we can work with
  floExtractSet <- st_collection_extract(floSplitSet, type = "LINESTRING")

  floExtractSet <- getFLOSegmentLengths(floExtractSet)

  
  #Initialize a data frame to store our results
  newColNames <- c("time_to_center")
  floRowCount <- nrow(floSet)
  floTimes <- data.frame(matrix(ncol = length(newColNames), nrow = floRowCount))
  colnames(floTimes) <- newColNames
  
  #Find our time to center of the buffer zone
  #Note: the "center", in this case, is simply the mid-point of the line inside the buffer zone, not the exact center of the circle
  for(floIndex in 1:floRowCount){
    partIDLoc <- getNumValueFromSFDF(floSet, floIndex, "conversion_to_partidloc_")
    floSegments <- floExtractSet[floExtractSet$conversion_to_partidloc_ == partIDLoc, ]
    
    #Get the length of our 1st segment
    segment1Length <- getNumValueFromSFDF(floSegments, 1, "segment_length")
    
    #3 Segments
    if(nrow(floSegments) == 3) {
      segment2Length <- getNumValueFromSFDF(floSegments, 2, "segment_length")
      distToCenter <- (segment1Length + (segment2Length/2)) #We'll say the distance to the center is the length of the first segment plus have the length of the segment inside the buffer zone. It's imperfect, but a decent approximation
    } else { #2 Segments, or any other weird scenarios
      distToCenter <- segment1Length #if we don't have 3 segments, just default to only the length of the first segment
    }
    
    totalFLOTime <- getNumValueFromSFDF(floSegments, 1, "time") #the time and total length columns are the same for all, so just pull from row 1
    totalFLOLength <- getNumValueFromSFDF(floSegments, 1, "total_length")

    timeToCenter <- (totalFLOTime * (distToCenter / totalFLOLength))
    
    floTimes[floIndex,"time_to_center"] <- timeToCenter 
  }
  
  floSet <- cbind(floSet, floTimes)
  return(floSet)
}

#' Finds the flow lines that intersect with our buffer zone
#' @param coordBufferZone a polygon object for the buffer zone in question 
#' @param floDataSet linestring objects, flow lines from our MODPATH model output
#' @returns a data frame of FLOs
getFLOsInBufferZone <- function(coordBufferZone, floDataSet) {
  intersections <- st_intersects(coordBufferZone, floDataSet)
  flosInBufferZone <- floDataSet[intersections[[1]], ]
  flosInBufferZone <- getFLOTimes(flosInBufferZone, coordBufferZone)
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


#' Get the CropScape land use mix for our starting points
#' @param stpDataSet a data frame of STPs from our MODPATH model
#' @param stpIDs a data frame of STP IDs
#' @param timeFrameOfInterest a time in years
#' @returns a data frame of land cover
getLandCoverMixCropScape <- function(stpDataSet, stpIDs, timeFrameOfInterest) {
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

#' Summarize the contributing zones by landuse mix for CropScape
#' @param landCoverMix a data frame with one row per STP ID and a column for land cover
#' @returns a data frame with one row per land cover, a column for the Count of particle IDs with that land cover, and a column with the relative frequency of that land use
getSummarizedLandCoverMixCropScape <- function(landCoverMix) {
  summarizedLandCoverMix <- landCoverMix %>%
    group_by(CLASS_NAME) %>%
    summarise(
      CDL_2022_Count = n(),
      CDL_2022_Relative = n() / nrow(landCoverMix)
    )
  return(summarizedLandCoverMix)
}

#' Calculates the estimated nitrate levels given a CropScape land cover mix
#' t15
#' @param landCoverMix a data frame with one row per particle IDs and a column for land cover
#' @returns a list with the following structure:
#'            summarizedLandCoverMix - a data frame with one row per land cover and columns for the amount of each land cover
#'            no3Prediction - a data frame with the no3 prediction. It has the fit value, as well as the prediction interval lower and upper bound
getEstimatedNitrateLevelsCropScape <- function(landCoverMix) {
  #Get our Summarized Land Cover Mix
  #
  summarizedLandCoverMix <- getSummarizedLandCoverMixCropScape(landCoverMix)
  
  #Create our prediction interval
  load("ztreyLinearModel.RData") #named cdlModel
  
  currentCorn <- summarizedLandCoverMix$CDL_2022_Count[summarizedLandCoverMix$CLASS_NAME=="Corn"]
  if (length(currentCorn) == 0) {currentCorn <- 0}
  
  currentSweetCorn <- summarizedLandCoverMix$CDL_2022_Count[summarizedLandCoverMix$CLASS_NAME=="Sweet Corn"]
  if (length(currentSweetCorn) == 0) {currentSweetCorn <- 0}
  
  currentPotato <- summarizedLandCoverMix$CDL_2022_Count[summarizedLandCoverMix$CLASS_NAME=="Potatoes"]
  if (length(currentPotato) == 0) {currentPotato <- 0}
  
  currentValues <- data.frame(xCorn = currentCorn, xSweetCorn = currentSweetCorn, xPotato = currentPotato)
  currentValues <- currentValues * 9.88 #this scales from "number of contributing zones" to "area". Needed since our model is in area
  
  no3Prediction <- predict.lm(cdlModel, currentValues, interval = "prediction", level = 0.95)
  
  no3Prediction <- as.data.frame(no3Prediction) #convert to data frame for easier handling later on
  
  #ztrey - left off here. Combine no3Prediction and summarizedLandCoverMix into a list and pass them back up the stack.
  #Then display the NO3 level estimation. I think add this to the Bar Chart interpretation, and just give the range
  
  estimatedNitrateLevelsReturnList <- list(summarizedLandCoverMix = summarizedLandCoverMix, no3Prediction = no3Prediction)
  
  return(estimatedNitrateLevelsReturnList)
}

#' Creates a histogram for the time to center for each of the FLOs
#' @param floTimes a data frame of flow times
#' @returns a histogram
createFlowTimeHistogram <- function(floTimes) {
  #Prepare our data
  floTimes <- floTimes %>%
    mutate(time_in_years = (time_to_center / 365.24))
  
  #Create our histogram
  flowTimeHistogram <- floTimes %>%
    ggplot(aes(x = time_in_years)) +
    geom_histogram(fill = "blue", color = "black") +
    labs(title = "Transit Times from Contributing Zones",
         x = "Ground Water Transit Times (years)",
         y = "Count of Pathlines") +
    theme(title = element_text(size = 19),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))
  return(flowTimeHistogram)
}

#' Wrapper function to create various plots
#' @param nitrateEstimatorReturnList a list of other objects that stores information to plot
#' @param landCoverCode the code for the land cover data set to use
#' @returns nitrateEstimatorReturnList with a few additional indices:
#'                        landCoverPlot - a stacked bar chart of the contributing zone land cover
#'                        flowTimeHistogram - a histogram of the flow times
createPlots <- function(nitrateEstimatorReturnList,landCoverCode) {
  #Create our plots
  landCoverBarPlot <- createLandCoverPlotWrapper(nitrateEstimatorReturnList$landCover, landCoverCode)
  flowTimeHistogram <- createFlowTimeHistogram(nitrateEstimatorReturnList$floIDs)
  
  #add them to our return list
  nitrateEstimatorReturnList$landCoverBarPlot <- landCoverBarPlot
  nitrateEstimatorReturnList$flowTimeHistogram <- flowTimeHistogram
  
  #output
  return(nitrateEstimatorReturnList)
}

#' Create a stacked bar chart of CropScape land covers
#' @param landCover a data frame of the landuse mix
#' @returns a stacked bar chart contributing zone land cover
createLandCoverPlotCropScape <- function(landCover) {
  #Stacked bar grouping
  #t014 I've got options around: 1) should we group individual land covers? 2) should we make a pareto chart or hold specific categories in place?
  #If I only stack some stuff, then i think color-coding the stacked stuff, and just having everything else be the same color is the way to go. No good to "double encode" with a label and color.
  landCoverStacked <- landCover %>%
    mutate(LandCoverCategory = case_when(
      CLASS_NAME %in% c("Corn", "Potatoes", "Sweet Corn") ~ "High Agriculture",
      CLASS_NAME %in% c("Deciduous Forest", "Mixed Forest", "Evergreen Forest",
                        "Woody Wetlands", "Herbaceous Wetlands",
                        "Shrubland", "Grassland/Pasture",
                        "Barren") ~ "Nature",
      TRUE ~ CLASS_NAME
    ))
  
  landCoverStacked <- landCoverStacked %>%
    group_by(LandCoverCategory) %>%
    mutate(CategoryCount = sum(CDL_2022_Relative, na.rm = TRUE)) %>%
    ungroup()

  landCoverStacked <- landCoverStacked %>%
    mutate(LandCoverCategory = fct_reorder(LandCoverCategory, CategoryCount, .desc = TRUE))
  
  stackPlotTitle = "Land Cover of Contributing Zones"
  stackedBarPlot <- landCoverStacked %>%
    ggplot(aes(x = LandCoverCategory, y = CDL_2022_Relative, fill = CLASS_NAME)) +
    geom_bar(stat = "identity") +
    labs(x = "Land Cover Category", y = "Percent", title = stackPlotTitle, fill = "Land Cover") +
    theme(title = element_text(size = 19),
          axis.title = element_text(size = 18),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 16)) + #tilt axis labels
    scale_y_continuous(labels = scales::label_percent())

  return(stackedBarPlot)
}

#' Create a stacked bar chart of WiscLand land covers
#' This is slightly different than CropScape, since our data is already grouped in a way that we like.
#' 
#' @param landCover a data frame of the landuse mix
#' @returns a stacked bar chart contributing zone land cover
createLandCoverPlotWiscLand <- function(landCover) {
  
  stackedPlotTitle = "Land Cover of Contributing Zones"
  stackedBarPlot <- landCover %>%
    ggplot(aes(x = cls_desc_1, y = LAND_COVER_COUNT, fill = cls_desc_3)) + #the bars will be Level 1, the subbars will be level 3
    geom_bar(stat = "identity") +
    labs(x = "Land Cover Category", y = "Count", title = stackedPlotTitle, fill = "Land Cover") +
    theme(title = element_text(size = 19),
          axis.title = element_text(size = 18),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 16))
  
  return(stackedBarPlot)
}

#' Wrapper function to create a stacked bar chart based on different land cover data sets
#' @param landCover a data frame of the landuse mix
#' @param landCoverCode the code for hte land cover data set to use
#' @returns a stacked bar chart contributing zone land cover
createLandCoverPlotWrapper <- function(landCover, landCoverCode) {
  if (landCoverCode == 1) {
    landCoverBarPlot <- createLandCoverPlotCropScape(landCover)
  } else if (landCoverCode == 2) {
    landCoverBarPlot <- createLandCoverPlotWiscLand(landCover)
  } else {
    landCoverBarPlot <- createLandCoverPlotCropScape(landCover) #default to cropscape as a default
  } 
  
  return(landCoverBarPlot)
}

#' Core logic for our nitrate estimator
#' @param coordsOfInterest the coordinates for which to estimate nitrate levels
#' @param timeFrameOfInterest the time frame over which we should look (in years)
#' @param buffer the radius of our buffer zone (in meters)
#' @param floDataSet the flow lines (FLOs) generated from our MODPATH model
#' @param stpDataSet the starting points (STPs) from our MODPATH model
#' @param landCoverCode the code for the land cover data set to use
#' @returns a list with the following structure:
#'            stpIDs: a data frame of contributing point IDs
#'            floIDs: a data frame of FLO IDs that intersect with our selected buffer zone
#'            landCover: Land Cover fraction of all of the contributing zones
#'            bufferZone: a polygon of our buffer zone
#'            no3Prediction: Estimated nitrate level
runNitrateEstimator <- function(coordsOfInterest, timeFrameOfInterest, buffer, floDataSet, stpDataSet,landCoverCode) {
  #Find contributing FLOs and STPs for the coordinates of interest
  contribSTPsForCoordReturnList <- getContribSTPsForCoord(coordsOfInterest, buffer, timeFrameOfInterest, floDataSet)
  contribSTPIDs <- contribSTPsForCoordReturnList$stpIDs
  contribFLOIDs <- contribSTPsForCoordReturnList$floIDs
  bufferZone <- contribSTPsForCoordReturnList$bufferZone
 
  # Find the land cover for the contributing points
  landCoverMix <- getLandCoverMixWrapper(stpDataSet, contribSTPIDs, timeFrameOfInterest, landCoverCode)
  
  # Find the estimated nitrogen impacts given the land use
  estimatedNitrateLevelsReturnList <- getEstimatedNitrateLevelsWrapper(landCoverMix, landCoverCode)
  landCoverSummary <- estimatedNitrateLevelsReturnList$summarizedLandCover
  no3Prediction <- estimatedNitrateLevelsReturnList$no3Prediction
  
  # Organize and return data
  nitrateEstimatorReturnList <- list(stpIDs = contribSTPIDs, floIDs = contribFLOIDs,landCover = landCoverSummary, bufferZone = bufferZone, no3Prediction = no3Prediction)
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
  landCoverCode <- getLandCoverCode()
  
  # ----2.2 Read in data----
  floDataSet <- getFloDataSet()
  stpDataSet <- getStpDataSet()
  
  # ----2.3 Find Nitrate Estimates----
  nitrateEstimatorReturnList <- runNitrateEstimator(coordsOfInterest, timeFrameOfInterest, buffer, floDataSet, stpDataSet,landCoverCode)
  nitrateEstimatorReturnList <- createPlots(nitrateEstimatorReturnList,landCoverCode)
  print(nitrateEstimatorReturnList$landCoverBarPlot)
  print(nitrateEstimatorReturnList$flowTimeHistogram)
  print(nitrateEstimatorReturnList$no3Prediction)
}

