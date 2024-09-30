#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Define UI for application
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(width = 6,
        p("click or drag the marker withint the bounded region to get nitrate estimates"),
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
