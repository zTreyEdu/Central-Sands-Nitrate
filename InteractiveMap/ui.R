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
          htmlOutput("chartExplainer")
          )
      ),
    fluidRow(
      box(width = 6,
          leafletOutput(outputId = "map", height = "500px")
          ),
      box(width = 6,
          plotOutput(outputId = "landCoverBarPlot")
          ),
      box(width = 6,
          plotOutput(outputId = "flowTimeHistogram")
          )
      ),
    fluidRow(
      box(htmlOutput("takeAction")),
      box(htmlOutput("externalLinks"))
      ),
    fluidRow(
      box(width = 6,
          htmlOutput("modelAssumptions")
          ),
      box(width = 6,
          dataTableOutput("flowlineInfoTable"))
      )
    ),
  title = "Interactive Groundwater Flow Map"
)
