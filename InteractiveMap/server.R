#Purpose: Server logic of the Interactive Map

#'Function for shiny server code
function(input, output, session) {
  #Initial call to create map
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = defaultLng, lat = defaultLat, zoom = 8) %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% #Aerial Photo Base Layer
      addProviderTiles("Stadia.StamenTerrain", group = "Terrain") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Default") %>% #Default Base Layer
      addCircleMarkers(data = pumpingWells,
                  group = "Pumping Wells",
                  radius = 5,
                  color = "blue",
                  fillColor = "green",
                  fillOpacity = 0.7) %>% ##Add pumping wells
      addArrowhead(data = floDisplaySet,
                  group = "Flow Lines",
                  color = "blue",
                  weight = 2,
                  opacity = 0.5) %>% #Add all flow lines
      hideGroup(c("Pumping Wells", "Flow Lines")) %>% #hide these groups by default
      addLayersControl(baseGroups = c("Default", "Aerial", "Terrain"),
                       overlayGroups = c("Pumping Wells", "Flow Lines"),
                       options = layersControlOptions(collapsed = TRUE)) %>% #Add toggle-able base layers
      addPolygons(data = pathLineBoundary,
                  group = "static",
                  color = "black",
                  fillColor = ~fill_color,
                  fillOpacity = 0.7) %>% #adds our pathLineBoundary shape
      addMarkers(lng = defaultLng, lat = defaultLat, options = markerOptions(draggable = TRUE), group = "marker") #create a moveable map marker
  })

  #Set up our Reactives----
  nitrateEstimateReactive <- reactiveValues(nitrateEstimatorReturnList = generateNitrateEstimates(defaultLng, defaultLat))
  
  current_marker <- reactiveValues(
    lng = defaultLng,
    lat = defaultLat
  )

  nO3Prediction <- reactive({
    #Return default values when the values aren't populated (e.g., when the app first starts up)
    if (is.null(nitrateEstimateReactive$nitrateEstimatorReturnList)) {
      return(list(fit = 0, lwr = 0, upr = 0 ))
    }
    
    nitrateEstimateReactive$nitrateEstimatorReturnList$no3Prediction
  })
  
  #Feature Switches
  reactiveBufferZone <- reactive({
    if (dispayBufferZone == 1) {
      return(st_transform(nitrateEstimateReactive$nitrateEstimatorReturnList$bufferZone, crs = 4326))
    }
    return(NULL)  # Return NULL if not to display
  })
  
  reactiveContribFLOs <- reactive({
    if (displayContribFLOs == 1) {
      return(getFLOProjection(nitrateEstimateReactive$nitrateEstimatorReturnList$floIDs))
    }
    return(NULL)
  })
  
  reactiveContribSTPs <- reactive({
    if (displayContribSTPs == 1) {
      return(getSTPCoords(nitrateEstimateReactive$nitrateEstimatorReturnList$stpIDs))
    }
    return(NULL)
  })
  
  #Nitrate Plots----
  output$landCoverBarPlot <- renderPlot({nitrateEstimateReactive$nitrateEstimatorReturnList$landCoverBarPlot})
  output$flowTimeHistogram <- renderPlot({nitrateEstimateReactive$nitrateEstimatorReturnList$flowTimeHistogram})
  
  #Table Output----
  output$flowlineInfoTable <- renderDataTable({
    testFLODF <- as.data.frame(nitrateEstimateReactive$nitrateEstimatorReturnList$floIDs)
    testFLODF[ ,1:8]
    })
  
  
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
      clearGroup("marker") %>% #only clear shapes we're drawing for each point, and keep our static border shape
      addMarkers(data = data.frame(lat = current_marker$lat, lng = current_marker$lng),
                 options = markerOptions(draggable = TRUE), group = "marker")

    #Update our UI with our Nitrate Plot
    nitrateEstimateReactive$nitrateEstimatorReturnList <- generateNitrateEstimates(current_marker$lng, current_marker$lat)

        #If our buffer zone didn't have any FLO intersections, then let the user know and quit out of this function
    if(nrow(nitrateEstimateReactive$nitrateEstimatorReturnList$stpIDs) == 0) {
      showNotification("Error: the model does not have any data for flow lines for this area. Please select another area.", id = "no_flowlines", type = "error")
      return()
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
      clearGroup("marker") %>% #only clear shapes we're drawing for each point, and keep our static border shape
      addMarkers(data = data.frame(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng),
                 options = markerOptions(draggable = TRUE), group = "marker")
    
    #Update our UI with our Nitrate Plot
    nitrateEstimateReactive$nitrateEstimatorReturnList <- generateNitrateEstimates(current_marker$lng, current_marker$lat)

    #If our buffer zone didn't have any FLO interesections, then let the user know and quit out of this function
    if(nrow(nitrateEstimateReactive$nitrateEstimatorReturnList$stpIDs) == 0) {
      showNotification("Error: the model does not have any data for flow lines for this area. Please select another area.", id = "no_flowlines", type = "error")
      return()
    }
    
    #Remove spinner
    hidePageSpinner()
  })
  
  #Feature Switch Observer----
  observe({
    # Get the data for each feature
    bufferZone <- reactiveBufferZone()
    contribFLOs <- reactiveContribFLOs()
    contribSTPs <- reactiveContribSTPs()
    
    # Handle Buffer Zone Layer
    if (!is.null(bufferZone)) {
      leafletProxy(mapId = "map") %>%
        clearGroup("dynamic") %>%  # Clear previous "dynamic" layers
        addPolygons(data = bufferZone,
                    group = "dynamic",
                    color = "red",
                    opacity = 0.25,
                    fillColor = "grey",
                    fillOpacity = 0.85)
    } else {
      leafletProxy(mapId = "map") %>%
        clearGroup("dynamic")  # If no buffer zone, remove the group
    }
    
    # Handle ContribFLOs Layer
    if (!is.null(contribFLOs)) {
      leafletProxy(mapId = "map") %>%
        addAntpath(data = contribFLOs,
                   group = "dynamic",
                   color = "blue",
                   weight = 3,
                   opacity = 0.5,
                   options = antpathOptions(delay = 2000))
    } else {
      leafletProxy(mapId = "map") %>%
        clearGroup("dynamic")  # If no FLOs, remove the group
    }
    
    # Handle ContribSTPs Layer
    if (!is.null(contribSTPs)) {
      leafletProxy(mapId = "map") %>%
        addCircleMarkers(data = contribSTPs,
                         group = "dynamic",
                         lng = ~lng,
                         lat = ~lat,
                         color = "orange",
                         radius = 5)
    } else {
      leafletProxy(mapId = "map") %>%
        clearGroup("dynamic")  # If no STPs, remove the group
    }
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
    no3fit <- format(round(nO3Prediction()$fit, digits = 1), nsmall = 1) #intentionally not using the point estimate prediction, as it projects too much certainty
    no3lwr <- format(round(nO3Prediction()$lwr, digits = 1), nsmall = 1)
    no3upr <- format(round(nO3Prediction()$upr, digits = 1), nsmall = 1)
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
