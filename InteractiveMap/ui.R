#Purpose: code for the UI for the Interactive Map, a shiny app
#
# Define UI for application
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(width = 6,
          p("Click or drag the marker within the bounded region to get estimated nitrate contribution from contributing zones."),
          leafletOutput(outputId = "map")
          ),
      box(width = 6,
          plotOutput(outputId = "landCoverBarPlot")
          )
      ),
      fluidRow(
      box(width = 6,
          htmlOutput("chartExplainer")
          ),
      box(width = 6,
          htmlOutput("coordInfo")
          )
      ),
      fluidRow(
        column(4,
      box(width = 6,
          htmlOutput("externalLinks")
          )),
      column(4,
      box(width = 6,
          htmlOutput("takeAction")
          )),
      column(4,
      box(width = 6,
          htmlOutput("appExplainer")
          ))
      )
    ),
    
  title = "Interactive Groundwater Flow Map"
)
