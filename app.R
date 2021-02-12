#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(UpsideMaximizer)

ui <- fluidPage(

    # Application title
    titlePanel("Upside Maximizer Helper"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("stock_id",
                      "Stock ticker",
                      value = "AG"),
            dateInput("start_date",
                      "Start date",
                      value = "2020-08-01"),
            sliderInput("sell_range",
                        "Range",
                        min = 0,
                        max = 1,
                        value = c(0.2, 0.4))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("UM_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # use UpsideMaximizer
    UM_object <- reactive({
        upside.maximizer(stocks = input$stock_id,
                         from = input$start_date,
                         span = 11,
                         sell_upper = input$sell_range[1],
                         sell_lower = input$sell_range[2])
    })
    
    output$UM_plot <- renderPlot({
        ggplot(data = UM_object()$data, aes(x = date, y = close)) +
            geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
