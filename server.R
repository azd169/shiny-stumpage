library(shiny)
library(tidyverse)
library(plotly)

server <- function(input, output, session) { # Add the session parameter
  
  # Load and preprocess the dataset
  stumpage <- read.csv("https://raw.githubusercontent.com/azd169/R-datasets/main/stumpage.csv", header = TRUE)
  
  impute_median <- function(x) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    return(x)
  }
  
  stumpage_imputed <- stumpage %>%
    group_by(Type) %>%
    mutate(Price = ifelse(is.na(Price), median(Price, na.rm = TRUE), Price)) %>%
    ungroup() # Ensure the grouping is removed after the mutation
  
  # Create the dynamic UI for the type selector
  output$type_selector <- renderUI({
    checkboxGroupInput("selected_type", "Select Type(s):", 
                       choices = unique(stumpage_imputed$Type), 
                       selected = unique(stumpage_imputed$Type))
  })
  
  # Update the selected types
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "selected_type", selected = unique(stumpage_imputed$Type))
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_type", selected = character(0))
  })
  
  # Reactive expression to filter data based on selected types
  filtered_data <- reactive({
    if (is.null(input$selected_type) || length(input$selected_type) == 0) {
      return(NULL)
    }
    stumpage_imputed %>% filter(Type %in% input$selected_type)
  })
  
  # Render the plot or a message based on the selected types
  output$plot_ui <- renderUI({
    if (is.null(input$selected_type) || length(input$selected_type) == 0) {
      return(tags$div(
        tags$h3("No types selected. Please select at least one type to display the plot."),
        style = "color: red; text-align: center; margin-top: 20px;"
      ))
    } else {
      plotlyOutput("plot", height = "700px")
    }
  })
  
  output$plot <- renderPlotly({
    data <- filtered_data()
    
    # Check if there's any data to plot
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    p1 <- ggplot(data,
                 aes(x = Time,
                     y = Price,
                     group = Type,
                     color = Type,
                     shape = Type,
                     text = paste("Type:", Type, "<br>Time:", Time, "<br>Price ($/ton):", Price)))  +
      geom_line(linewidth = 1) +          
      geom_point(size = 3) +      
      labs(y = "Price ($/ton)") + 
      scale_shape_manual(values = c(8, 21, 22, 23, 24, 25), name = "Type") +
      theme(strip.text = element_text(size = 16),
            legend.title = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = "lightgray", linetype = "dashed"), 
            legend.text = element_text(size = 16),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 16),
            axis.line = element_line(color = "black"),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") # Move legend to the bottom in ggplot
    
    plotly_plot <- ggplotly(p1, tooltip = "text")
    
    # Adjust the layout to move the legend to the bottom in Plotly and maximize plot area
    plotly_plot <- plotly_plot %>%
      layout(legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
             margin = list(l = 50, r = 50, t = 50, b = 100))
    
    plotly_plot
  })
}

# Run the application
shinyApp(ui = ui, server = server)