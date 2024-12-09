#Purpose: Server logic of the Interactive Map

#'Function for shiny server code
function(input, output, session) {
  #Initial call to create map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -89.518247, lat = 44.210243, zoom = 8) %>% #center around Plainsfield, WI
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% #Aerial Photo Base Layer
      addProviderTiles("Stadia.StamenTerrain", group = "Terrain") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Default") %>% #Default Base Layer
      addLayersControl(baseGroups = c("Default", "Aerial", "Terrain"),
                       options = layersControlOptions(collapsed = TRUE)) %>% #Add toggle-able base layers
      addPolygons(data = pathLineBoundary,
                  color = "black",
                  fillColor = ~fill_color,
                  fillOpacity = 0.7) %>% #adds our pathLineBoundary shape
      addMarkers(lng = -89.518247, lat = 44.210243, options = markerOptions(draggable = TRUE)) #create a moveable map marker
  })
  
  #Set up our reactive values----
  current_marker <- reactiveValues(
    lng = -89.518247,
    lat = 44.210243
  )
  
  nO3Prediction <- reactiveValues(
    fit = 0,
    lwr = 0,
    upr = 0
  )
  
  #Allow user to drag the map marker----
  observeEvent(input$map_marker_dragend, {
    
    #Show a spinner to let the user know that something is happening
    showPageSpinner(caption = "Calculating...")
    
    #Check that the marker is in bounds---
    regionData <- getRegionData(pathLineBoundary = pathLineBoundary,
                      marker = data.frame(lat = input$map_marker_dragend$lat, lng = input$map_marker_dragend$lng))
    if(nrow(regionData) == 0) {
      showNotification("Error: Marker is out of bounds. Please select an area within the bounded region.", id = "region_error", type = "error")
    } else {
      current_marker$lat <- input$map_marker_dragend$lat
      current_marker$lng <- input$map_marker_dragend$lng
    }

    #Update the map with our new marker locations
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      clearGroup("dynamic") %>% #only clear shapes we're drawing for each point, and keep our static border shape
      addMarkers(data = data.frame(lat = current_marker$lat, lng = current_marker$lng),
                 options = markerOptions(draggable = TRUE))

    #Update our UI with our Nitrate Plot
    nitrateEstimatorReturnList <- generateNitrateEstimates(current_marker$lng, current_marker$lat)
    output$landCoverBarPlot <- renderPlot({nitrateEstimatorReturnList$landCoverBarPlot})
    output$flowTimeHistogram <- renderPlot({nitrateEstimatorReturnList$flowTimeHistogram})
    
    #If our buffer zone didn't have any FLO intersections, then let the user know and quit out of this function
    if(nrow(nitrateEstimatorReturnList$stpIDs) == 0) {
      showNotification("Error: the model does not have any data for flow lines for this area. Please select another area.", id = "no_flowlines", type = "error")
      return()
    }
    
    #Update Nitrate Prediction Reactive
    nO3Prediction$fit <- nitrateEstimatorReturnList$no3Prediction$fit
    nO3Prediction$lwr <- nitrateEstimatorReturnList$no3Prediction$lwr
    nO3Prediction$upr <- nitrateEstimatorReturnList$no3Prediction$upr
    
    #See which of our optional features to enable
    if(dispayBufferZone == 1) {
      bufferZone <- st_transform(nitrateEstimatorReturnList$bufferZone, crs = 4326)
      leafletProxy(mapId = "map") %>%
        addPolygons(data = bufferZone,
                    group = "dynamic",
                    color = "red",
                    opacity = 0.25,
                    fillColor = "grey",
                    fillOpacity = 0.85)
    }

    if(displayContribFLOs == 1) {
      projectedFLODIDs <- getFLOProjection(nitrateEstimatorReturnList$floIDs)
      leafletProxy(mapId = "map") %>%
        addAntpath(data = projectedFLODIDs,
                       group = "dynamic",
                       color = "blue",
                       weight = 3,
                       opacity = 0.5,
                   options = antpathOptions(delay = 2000))
    }
    
    if(displayContribSTPs == 1) {
      stpCoords <- getSTPCoords(nitrateEstimatorReturnList$stpIDs)
      leafletProxy(mapId = "map") %>%
        addCircleMarkers(data = stpCoords,
                         lng = ~lng,
                         lat = ~lat,
                         color = "orange",
                         radius = 5)
    }
    
    #Remove spinner
    hidePageSpinner()
  })
  
  #Alternatively, allow user to click on the map----
  observeEvent(input$map_shape_click, {
    #Show a spinner to let the user know that something is happening
    showPageSpinner(caption = "Calculating...")
    
    #Clear existing markers and shapes, and update map with new marker location
    current_marker$lat <- input$map_shape_click$lat
    current_marker$lng <- input$map_shape_click$lng

    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      clearGroup("dynamic") %>% #only clear shapes we're drawing for each point, and keep our static border shape
      addMarkers(data = data.frame(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng),
                 options = markerOptions(draggable = TRUE))
    
    #Update our UI with our Nitrate Plot
    nitrateEstimatorReturnList <- generateNitrateEstimates(current_marker$lng, current_marker$lat)
    output$landCoverBarPlot <- renderPlot({nitrateEstimatorReturnList$landCoverBarPlot})
    output$flowTimeHistogram <- renderPlot({nitrateEstimatorReturnList$flowTimeHistogram})
    
    #If our buffer zone didn't have any FLO interesections, then let the user know and quit out of this function
    if(nrow(nitrateEstimatorReturnList$stpIDs) == 0) {
      showNotification("Error: the model does not have any data for flow lines for this area. Please select another area.", id = "no_flowlines", type = "error")
      return()
    }
    
    #Update Nitrate Prediction Reactive
    nO3Prediction$fit <- nitrateEstimatorReturnList$no3Prediction$fit
    nO3Prediction$lwr <- nitrateEstimatorReturnList$no3Prediction$lwr
    nO3Prediction$upr <- nitrateEstimatorReturnList$no3Prediction$upr
    
    #See which of our optional features to enable
    if(dispayBufferZone == 1) {
      bufferZone <- st_transform(nitrateEstimatorReturnList$bufferZone, crs = 4326)
      leafletProxy(mapId = "map") %>%
        addPolygons(data = bufferZone,
                    group = "dynamic",
                    color = "red",
                    opacity = 0.25,
                    fillColor = "grey",
                    fillOpacity = 0.85)
    }
    
    if(displayContribFLOs == 1) {
      projectedFLODIDs <- getFLOProjection(nitrateEstimatorReturnList$floIDs)
      leafletProxy(mapId = "map") %>%
        addAntpath(data = projectedFLODIDs,
                       group = "dynamic",
                       color = "blue",
                       weight = 3,
                       opacity = 0.5,
                   options = antpathOptions(delay = 2000))
    }
    
    if(displayContribSTPs == 1){
      stpCoords <- getSTPCoords(nitrateEstimatorReturnList$stpIDs)
      leafletProxy(mapId = "map") %>%
        addCircleMarkers(data = stpCoords,
                         lng = ~lng,
                         lat = ~lat,
                         color = "orange",
                         radius = 5)
    }
    
    #Remove spinner
    hidePageSpinner()
  })
  
  #Pass some output text to the UI
  output$mapExplainer <- renderText({
    displayLat <- format(round(current_marker$lat, digits = 7), nsmall = 7) #round to 7 decimal points, and format to display trailing 0s
    displayLng <- format(round(current_marker$lng, digits = 7), nsmall = 7) #round to 7 decimal points, and format to display trailing 0s
    paste0(h2("Map Explanation"),
           "Click or drag the marker within the bounded region to get estimated land use from groundwater contributing zones.", "<br>",
           "The purple dots represent simulated groundwater entry points.", "<br>",
           "The blue lines represent the modeled groundwater flow.", "<br>",
           "Current marker latitude: ", displayLat, "<br>",
           "Current marker longitude: ", displayLng, "<br>")
  })
  output$chartExplainer <- renderText({
    no3fit <- format(round(nO3Prediction$fit, digits = 1), nsmall = 1) #intentionally not using the point estimate prediction, as it projects too much certainty
    no3lwr <- format(round(nO3Prediction$lwr, digits = 1), nsmall = 1)
    no3upr <- format(round(nO3Prediction$upr, digits = 1), nsmall = 1)
    no3Units <- "mg/L"
    paste0(h2("Chart Explanation"),
           "This bar chart shows the break down of land cover for the groundwater entry points", "<br>",
           "The nitrate level for the region you selected is likely between ", no3lwr, " ", no3Units, " and ", no3upr, " ", no3Units)
  })
  output$externalLinks <- renderText({
    paste0(h2("Additional Info"),
           tags$ul(
             tags$li(a(href ="https://www.epa.gov/mn/what-nitrate", "Learn about Nitrate from the Environmental Protection Agency")),
             tags$li(a(href = "https://www3.uwsp.edu/cnr-ap/watershed/Pages/default.aspx", "See more maps at the UW-Stevens Point Center for Watershed Science and Education")),
             tags$li("Learn about the modeling software used: "),
             tags$ul(
               tags$li(a(href = "https://www.usgs.gov/mission-areas/water-resources/science/modflow-and-related-programs", "MODFLOW")),
               tags$li(a(href = "https://www.usgs.gov/software/modpath-particle-tracking-model-modflow", "MODPATH"))
               )
             )
           )
  })
  output$takeAction <- renderText({
    paste0(h2("Action"),
           "If your well has a high percentage of agricultural contributing zones, we recommend you test your well at least once a --time--.",
           "You can order a test here: --link out to website to order testing--"
           )
  })
  
  output$modelAssumptions <- renderText({
    paste0(h2("Model Assumptions"),
           "There are many assumptions that go into the Nitrate estimates from this model. This model makes the following assumptions and simplifications:",
           tags$li("It assumes uniform nitrate application in an area."),
           tags$li("It assumes land cover is an exact proxy for nitrate application."),
           tags$li("It assumes land cover has remained constant over time."),
           tags$li("It does not account  for the time or distance of the groundwater flow."),
           tags$li("It does not account for groundwater depth."),
           tags$li("It assumes CropScape is accurate."),
           tags$li("It only uses groundwater as a predictor, and does not account for the effects of precipitation or runoff."),
           tags$li("It assumes a constant soil porosity and does not account for different soil types."),
           tags$li("It assumes steady-state nitrate application and groundwater flow."),
           tags$li("Only the land cover of the contributing points is considered; land cover in between the contributing zones and selected regions is not accounted for."),
           tags$li("The selected region is buffered to a circle with a 100 meter radius.")
           )
  })
}
