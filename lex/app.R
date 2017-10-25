#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

df <- read_rds("lex.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country",
                  "Select a country to plot:",
                  choices = sort(unique(df$NAME)), 
                  selected = "Russia")
    ),
    
    mainPanel(
      plotOutput("linechart")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  linechart_data <- reactive({
    
    out <- filter(df, NAME == input$country)
    
    return(out)
    
  })
   
   output$linechart <- renderPlot({
      # generate bins based on input$bins from ui.R
      ggplot(linechart_data(), aes(x = time, y = lex, color = sex)) + 
       geom_line(size = 1) + 
       theme_minimal(base_size = 14) + 
       scale_color_manual(values = c('darkred', 'navy')) + 
       labs(title = paste0("Life expectancy at birth since the fall of the USSR"), 
            subtitle = input$country, 
            caption = "Data source: US Census Bureau IDB via the idbr R package", 
            x = "Year", 
            y = "Life expectancy at birth", 
            color = "")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

