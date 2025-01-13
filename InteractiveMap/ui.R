#Purpose: code for the UI for the Interactive Map, a shiny app
#


# Define UI for application
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabsetPanel(
      id = "mainPanel",
      
      #First tab for Explaining
      tabPanel(
        title = "Interactive Map",
        fluidRow(
          box(width = 6, htmlOutput("mapExplainer")),
          box(width = 6, htmlOutput("chartExplainer"))
          ),
        fluidRow(
          box(width = 4, leafletOutput(outputId = "map", height = "500px")),
          box(width = 4, plotOutput(outputId = "landCoverBarPlot")),
          box(width = 4, plotOutput(outputId = "flowTimeHistogram"))
        )
        ),
      
      tabPanel(
        title = "Additional Information",
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
