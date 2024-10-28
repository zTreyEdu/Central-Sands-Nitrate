#Purpose: code for the UI for the Interactive Map, a shiny app
#
# Define UI for application
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(width = 6,
        p("Click or drag the marker within the bounded region to get nitrate estimates"),
        leafletOutput(outputId = "map")
        ),
    box(width = 6,
        htmlOutput("text")
        ),
    box(width = 6,
        plotOutput(outputId = "nitratePlot"))
    ),
  
  title = "Interactive Nitrate Map"
)
