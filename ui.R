#Purpose: code for the UI for the Interactive Map, a shiny app

# Define UI for application
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabsetPanel(
      id = "mainPanel",
      
      #First tab - Interactive Display----
      tabPanel(
        title = "Interactive Map",
        fluidRow(
          box(width = 4, withSpinner(leafletOutput(outputId = "map", height = "500px"), caption = "Rendering Map..."), htmlOutput("mapExplainer")),
          box(width = 4, withSpinner(plotOutput(outputId = "landCoverBarPlot", height = "500px"), caption = "Determining Land Cover..."), htmlOutput("landCoverExplainer")),
          box(width = 4, withSpinner(plotOutput(outputId = "flowTimeHistogram", height = "500px"), caption ="Processing Transit Times..."), htmlOutput("transitTimeExplainer"))
          ),
        fluidRow(
          box(htmlOutput("takeAction"))
          )
        ),
      
      #Second tab - Additional Info----
      tabPanel(
        title = "Additional Information",
        fluidRow(
          box(htmlOutput("externalLinks"))
          ),
        fluidRow(
          box(width = 6, htmlOutput("modelAssumptions"))
          )
        ),
      
      #Third tab - Flowline Details----
      tabPanel(
        title = "Flowline Details (Prototype)",
        fluidRow(
          box(width = 6, imageOutput("groundWaterImage"))
          ),
        fluidRow(
          box(width = 12, dataTableOutput("flowlineInfoTable"))
          ),
        fluidRow(
          box(width = 12, htmlOutput("flowlines3D"))
        )
        )
      )
    ),
  title = "Interactive Groundwater Flow Map"
)
