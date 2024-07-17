library(shiny)

ui <- fluidPage(
  titlePanel("Stumpage Prices Interactive Plot"),
  fluidRow(
    column(2,
           uiOutput("type_selector"), # Dynamic UI for selecting types
           actionButton("select_all", "Select All"),
           actionButton("deselect_all", "Deselect All")
    ),
    column(10,
           uiOutput("plot_ui") # Use uiOutput to conditionally display plot or message
    )
  )
)