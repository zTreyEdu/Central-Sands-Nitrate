#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#TODO

function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(pathLineBoundary) %>%
      setView(lng = -89.518247, lat = 44.210243, zoom = 8) %>% #center around plainsfield
      addTiles() %>%
      addPolygons(color = "black",
                  fillColor = ~fill_color,
                  fillOpacity = 0.7) %>%
      addMarkers(lng = -89.518247, lat = 44.210243, options = markerOptions(draggable = TRUE)) #create a moveable map marker
  })
  
  current_markers <- reactiveValues(
    lng = -89.518247,
    lat = 44.210243
  )
  
  observeEvent(input$map_marker_dragend, {
    
    rd <- region_data(pathLineBoundary = pathLineBoundary,
                      markers = data.frame(lat = input$map_marker_dragend$lat, lng = input$map_marker_dragend$lng))
    
    if(nrow(rd) == 0) {
      showNotification("Error: no data for this location", id = "region_error")
    } else {
      current_markers$lat <- input$map_marker_dragend$lat
      current_markers$lng <- input$map_marker_dragend$lng
    }
    
    #Update the map with our new marker locations
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      addMarkers(data = data.frame(lat = current_markers$lat, lng = current_markers$lng),
                 options = markerOptions(draggable = TRUE))
    
    nitrateBarPlot <- generateNitrateEstimates(current_markers$lng, current_markers$lat)
    output$nitratePlot <- renderPlot({nitrateBarPlot})

  })
  
  #this will activate once I add a shapefile of WI
  observeEvent(input$map_shape_click, {
    print(input$map_shape_click$lat)
    print(input$map_shape_click$lng)
    
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      addMarkers(data = data.frame(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng),
                 options = markerOptions(draggable = TRUE))
    
    current_markers$lat <- input$map_shape_click$lat
    current_markers$lng <- input$map_shape_click$lng
    
    nitrateBarPlot <- generateNitrateEstimates(current_markers$lng, current_markers$lat)
    output$nitratePlot <- renderPlot({nitrateBarPlot})
  })
  
  output$text <- renderText({
    paste0("Current marker latitude: ", current_markers$lat, " <br> ",
           "Current marker longitude: ", current_markers$lng, " <br> ")
  })

}
