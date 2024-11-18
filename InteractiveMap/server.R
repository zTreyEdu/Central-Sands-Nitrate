#Purpose: Server logic of the Interactive Map

#'Function for shiny server code
function(input, output, session) {
  #Initial call to create map
  output$map <- renderLeaflet({
    leaflet(pathLineBoundary) %>%
      setView(lng = -89.518247, lat = 44.210243, zoom = 8) %>% #center around Plainsfield, WI
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% #Aerial Photo Base Layer
      addProviderTiles("Stadia.StamenTerrain", group = "Terrain") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Default") %>% #Default Base Layer
      addLayersControl(baseGroups = c("Default", "Aerial", "Terrain"),
                       options = layersControlOptions(collapsed = TRUE)) %>% #Add toggle-able base layers
      addPolygons(color = "black",
                  fillColor = ~fill_color,
                  fillOpacity = 0.7) %>% #adds our pathLineBoundary shape
      addMarkers(lng = -89.518247, lat = 44.210243, options = markerOptions(draggable = TRUE)) #create a moveable map marker
  })
  
  #Set up our reactive values----
  current_marker <- reactiveValues(
    lng = -89.518247,
    lat = 44.210243
  )
  
  #Allow user to drag the map marker----
  observeEvent(input$map_marker_dragend, {
    
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
      clearGroup("floIDs") %>% #just remove our floIDs group, and keep our static border shape
      addMarkers(data = data.frame(lat = current_marker$lat, lng = current_marker$lng),
                 options = markerOptions(draggable = TRUE))

    #Update our UI with our Nitrate Plot
    nitrateEstimatorReturnList <- generateNitrateEstimates(current_marker$lng, current_marker$lat)
    output$landCoverBarPlot <- renderPlot({nitrateEstimatorReturnList$landCoverBarPlot})
    
    #If our buffer zone didn't have any FLO intersections, then let the user know and quit out of this function
    if(nrow(nitrateEstimatorReturnList$stpIDs) == 0) {
      showNotification("Error: the model does not have any data for flow lines for this area. Please select another area.", id = "no_flowlines", type = "error")
      return()
    }

    #See which of our optional features to enable
    if(displayContribFLOs == 1) {
      projectedFLODIDs <- getFLOProjection(nitrateEstimatorReturnList$floIDs)
      leafletProxy(mapId = "map") %>%
      addPolylines(data = projectedFLODIDs,
                     group = "floIDs",
                     color = "blue",
                     weight = 3,
                     opacity = 0.5)
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

  })
  
  #Alternatively, allow user to click on the map----
  observeEvent(input$map_shape_click, {
    #Clear existing markers and shapes, and update map with new marker location
    current_marker$lat <- input$map_shape_click$lat
    current_marker$lng <- input$map_shape_click$lng

    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      clearGroup("floIDs") %>% #just remove our floIDs group and keep our static border shape
      addMarkers(data = data.frame(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng),
                 options = markerOptions(draggable = TRUE))
    
    #Update our UI with our Nitrate Plot
    nitrateEstimatorReturnList <- generateNitrateEstimates(current_marker$lng, current_marker$lat)
    output$landCoverBarPlot <- renderPlot({nitrateEstimatorReturnList$landCoverBarPlot})
    
    #If our buffer zone didn't have any FLO interesections, then let the user know and quit out of this function
    if(nrow(nitrateEstimatorReturnList$stpIDs) == 0) {
      showNotification("Error: the model does not have any data for flow lines for this area. Please select another area.", id = "no_flowlines", type = "error")
      return()
      }
    
    #See which of our optional features to enable
    if(displayContribFLOs == 1) {
      projectedFLODIDs <- getFLOProjection(nitrateEstimatorReturnList$floIDs)
      leafletProxy(mapId = "map") %>%
      addPolylines(data = projectedFLODIDs,
                     group = "floIDs",
                     color = "blue",
                     weight = 3,
                     opacity = 0.5)
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
  })
  
  #Pass some output text to the UI
  output$coordInfo <- renderText({
    paste0("Current marker latitude: ", current_marker$lat, "<br>",
           "Current marker longitude: ", current_marker$lng, "<br>")
  })
  output$mapExplainer <- renderText({
    paste0(h2("Map Explanation"),
           "The purple dots represent simulated groundwater entry points.", "<br>",
           "The blue lines represent the modeled groundwater flow.")
  })
  output$chartExplainer <- renderText({
    paste0(h2("Chart Explanation"),
           "This bar chart shows the break down of land cover for the groundwater entry points")
  })
  output$externalLinks <- renderText({
    paste0(h2("Additional Info"),
           "* ", a(href ="https://www.epa.gov/mn/what-nitrate", "Learn about Nitrate from the Environmental Protection Agency"), "<br>",
           "* ", a(href = "https://www3.uwsp.edu/cnr-ap/watershed/Pages/default.aspx", "See more maps at the UW-Stevens Point Center for Watershed Science and Education"), "<br>",
           "* Learn about the modeling software used: ",
           a(href = "https://www.usgs.gov/mission-areas/water-resources/science/modflow-and-related-programs", "MODFLOW"),
           " and ",
           a(href = "https://www.usgs.gov/software/modpath-particle-tracking-model-modflow", "MODPATH"))
  })
  output$takeAction <- renderText({
    paste0(h2("Action"),
           "If your well has a high percentage of agricultural contributing zones, we recommend you test your well at least once a --time--.",
           "You can order a test here: --link out to website to order testing--"
           )
  })
}
