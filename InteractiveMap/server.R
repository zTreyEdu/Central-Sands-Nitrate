#Purpose: Server logic of the Interactive Map, a shiny app

#TODO

#'Function for shiny server code
function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(pathLineBoundary) %>%
      setView(lng = -89.518247, lat = 44.210243, zoom = 8) %>% #center around plainsfield, WI
      addTiles() %>%
      addPolygons(color = "black",
                  fillColor = ~fill_color,
                  fillOpacity = 0.7) %>%
      addMarkers(lng = -89.518247, lat = 44.210243, options = markerOptions(draggable = TRUE)) #create a moveable map marker
  })
  
  current_marker <- reactiveValues(
    lng = -89.518247,
    lat = 44.210243
  )
  
  #Allow user to drag the map marker
  observeEvent(input$map_marker_dragend, {
    
    regionData <- getRegionData(pathLineBoundary = pathLineBoundary,
                      marker = data.frame(lat = input$map_marker_dragend$lat, lng = input$map_marker_dragend$lng))
    
    if(nrow(regionData) == 0) {
      showNotification("Error: no data for this location", id = "region_error")
    } else {
      current_marker$lat <- input$map_marker_dragend$lat
      current_marker$lng <- input$map_marker_dragend$lng
    }
    
    #Update our UI with our Nitrate Plot
    nitrateEstimatorReturnList <- generateNitrateEstimates(current_marker$lng, current_marker$lat)
    output$landCoverBarPlot <- renderPlot({nitrateEstimatorReturnList$landCoverBarPlot})
    stpCoords <- getSTPCoords(nitrateEstimatorReturnList$stpIDs)
    projectedFLODIDs <- getFLOProjection(nitrateEstimatorReturnList$floIDs)

    #Update the map with our new marker locations
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      clearGroup("floIDs") %>% #just remove our floIDs group, and keep our static border shape
      addMarkers(data = data.frame(lat = current_marker$lat, lng = current_marker$lng),
                 options = markerOptions(draggable = TRUE))


    #Feature switches
    if(displayContribSTPs == 1) {leafletProxy(mapId = "map") %>%
        addCircleMarkers(data = stpCoords,
                         lng = ~lng,
                         lat = ~lat,
                         color = "blue",
                         radius = 5)}
    if(displayContribFLOs == 1) {leafletProxy(mapId = "map") %>%
        addPolylines(data = projectedFLODIDs,
                     group = "floIDs",
                     color = "green",
                     weight = 3,
                     opacity = 0.8)}

  })
  
  #Alternatively, allow user to click on the map
  observeEvent(input$map_shape_click, {
    #Update map marker
    current_marker$lat <- input$map_shape_click$lat
    current_marker$lng <- input$map_shape_click$lng
    
    #Update our UI with our Nitrate Plot
    nitrateEstimatorReturnList <- generateNitrateEstimates(current_marker$lng, current_marker$lat)
    output$landCoverBarPlot <- renderPlot({nitrateEstimatorReturnList$landCoverBarPlot})
    stpCoords <- getSTPCoords(nitrateEstimatorReturnList$stpIDs)
    projectedFLODIDs <- getFLOProjection(nitrateEstimatorReturnList$floIDs)
    
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      clearGroup("floIDs") %>% #just remove our floIDs group, and keep our static border shape
      addMarkers(data = data.frame(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng),
                 options = markerOptions(draggable = TRUE))
    
    #Feature switches
    if(displayContribSTPs == 1){leafletProxy(mapId = "map") %>%
        addCircleMarkers(data = stpCoords,
                         lng = ~lng,
                         lat = ~lat,
                         color = "blue",
                         radius = 5)}
    if(displayContribFLOs == 1) {leafletProxy(mapId = "map") %>%
        addPolylines(data = projectedFLODIDs,
                     group = "floIDs",
                     color = "green",
                     weight = 3,
                     opacity = 0.8)}
  })
  
  
  #Pass some output text to the UI
  output$text <- renderText({
    paste0("Current marker latitude: ", current_marker$lat, " <br> ",
           "Current marker longitude: ", current_marker$lng, " <br> ")
  })

}
