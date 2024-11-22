#Purpose: code for the UI for the Interactive Map, a shiny app
#
# Define UI for application
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(width = 6,
          htmlOutput("mapExplainer")
          ),
      box(width = 6,
          htmlOutput("chartExplainer"))
    ),
    fluidRow(
      box(width = 6,
          leafletOutput(outputId = "map")
          ),
      box(width = 6,
          plotOutput(outputId = "landCoverBarPlot")
          )
      ),
      fluidRow(
        box(htmlOutput("takeAction")),
        box(htmlOutput("externalLinks"))
        )
    ),
  title = "Interactive Groundwater Flow Map"
)
