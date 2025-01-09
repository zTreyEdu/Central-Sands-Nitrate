#Purpose: code for the UI for the Interactive Map, a shiny app
#


# Define UI for application
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    navlistPanel(
      id = "mainPanel",
      well = TRUE,
      
      #First Panel for Explaining
      tabPanel(
        title = "Introduction",
        fluidRow(
          box(width = 6, htmlOutput("mapExplainer")),
          box(width = 6, htmlOutput("chartExplainer"))
          )
        ),
      
      tabPanel(
        title = "Analysis",
        fluidRow(
          box(width = 4, leafletOutput(outputId = "map", height = "500px")),
          box(width = 4, plotOutput(outputId = "landCoverBarPlot")),
          box(width = 4, plotOutput(outputId = "flowTimeHistogram"))
          ),
        fluidRow(
          box(htmlOutput("takeAction")),
          box(htmlOutput("externalLinks"))
          ),
        fluidRow(
          box(width = 6, htmlOutput("modelAssumptions")
          ),
          box(width = 6, dataTableOutput("flowlineInfoTable"))
          ),
        fluidRow(
          box(width = 6, imageOutput("groundWaterImage"))
          )
        )
      )
    ),
  title = "Interactive Groundwater Flow Map"
)
