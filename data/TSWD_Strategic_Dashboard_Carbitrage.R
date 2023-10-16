### Load Shiny Packages
library(shiny)
library(shinydashboard)

### Title and Logo

ui <- dashboardPage(
  dashboardHeader(title = "Subaru Strategic Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(12, h2("Subaru Strategic Dashboard"), align = "center"),
      column(12, img(src = "subaru_logo.png", height = 100, width = 100, align = "center"))
    )
  )
)
server <- function(input, output) { }

shinyApp(ui, server)

### Visualization 1: Hybrid vs Non Hybrid sales by location and odometer
ui <- dashboardPage(
  ##
  dashboardBody(
    fluidRow(
      column(12, h3("Hybrid vs Non-Hybrid Sales by Location and Odometer")),
      column(12, plotOutput("hybridPlot"))
    )
  )
)

server <- function(input, output) {
  output$hybridPlot <- renderPlot({
    # Create your hybrid vs non-hybrid plot here using ggplot or plotly
  })
}

shinyApp(ui, server)

### Visualization 2: Model Resale Price by Year
# Assuming 'resale_price' column exists in your data
ui <- dashboardPage(
  # ...
  dashboardBody(
    fluidRow(
      column(12, h3("Model Resale Price by Year")),
      column(12, plotOutput("resalePricePlot"))
    )
  )
)

server <- function(input, output) {
  output$resalePricePlot <- renderPlot({
    # Create your resale price plot using ggplot or plotly
  })
}

shinyApp(ui, server)

### Textbox at the Bottom
ui <- dashboardPage(
  # ...
  dashboardBody(
    # ... Previous components ...
    fluidRow(
      column(12, h3("About Subaru")),
      column(12, textOutput("subaruInfo"))
    )
  )
)

server <- function(input, output) {
  output$subaruInfo <- renderText({
    "Subaru is a well-known automobile manufacturer known for its reliable and versatile vehicles. 
    They offer a wide range of models, including Forester, Outback, Legacy, Crosstrek, Crosstrek Hybrid, 
    Solterra, Ascent, Impreza, and WRX. Subaru vehicles are popular for their safety features, 
    all-wheel-drive capabilities, and innovative technology."
  })
}

shinyApp(ui, server)


