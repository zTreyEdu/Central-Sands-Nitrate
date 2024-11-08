#Purpose: code for the UI for the Interactive Map, a shiny app
#
# Define UI for application
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(width = 6,
        p("Click or drag the marker within the bounded region to get estimated nitrate contribution from contributing zones."),
        leafletOutput(outputId = "map")
        ),
    box(width = 6,
        htmlOutput("coordInfo")
        ),
    box(width = 6,
        plotOutput(outputId = "landCoverBarPlot")
        )
    ),
  
  title = "Interactive Nitrate Map"
)
