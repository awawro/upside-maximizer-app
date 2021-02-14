library(shiny)
library(dplyr)
library(tidyquant)
library(ggplot2)
library(ggpmisc)
library(UpsideMaximizer)

ui <- fluidPage(

    titlePanel("Upside Maximizer Planner"),

    sidebarLayout(
        sidebarPanel(
            selectInput("stock_id",
                      "Stock ticker",
                      choices = c("AG", "GOLD", "NXE", "UUUU", "CCJ", "DNN"),
                      selected = "AG"),
            
            dateInput("start_date",
                      "Start date",
                      value = "2020-08-01"),
            
            fluidRow(
                column(6, sliderInput("min.pos", "Minimum no.", min = 1, max = 7, value = 1)),
                column(6, sliderInput("max.pos", "Maximum no.", min = 1, max = 7, value = 1))
            ),
            
            sliderInput("sell_range",
                        "Range",
                        min = 0,
                        max = 1,
                        value = c(0.2, 0.4),
                        step = 0.05)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("UM_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # use UpsideMaximizer
    UM_object <- reactive({
        UpsideMaximizer::upside.maximizer(stocks = input$stock_id,
                         from = input$start_date,
                         span_min = 11,
                         span_max = 11)
    })
    
    observeEvent(UM_object(), {
        updateSliderInput(session, "min.pos", max = nrow(UM_object()$loc.min))
    }) 
    
    observeEvent(UM_object(), {
        updateSliderInput(session, "max.pos", max = nrow(UM_object()$loc.max))
    }) 
    
    output$UM_plot <- renderPlot({
        
        df_min <- UM_object()$loc.min
        sel_min <- df_min[input$min.pos,]
        
        df_max <- UM_object()$loc.max
        sel_max <- df_max[input$max.pos,]
        
        lower_bound = sel_max$close - (sel_max$close - sel_min$close) * input$sell_range[1] 
        upper_bound = sel_max$close - (sel_max$close - sel_min$close) * input$sell_range[2]
        
        ggplot2::ggplot(data = UM_object()$data, aes(x = date, y = close)) +
            geom_line() +
            geom_point(data = UM_object()$loc.min, color = "blue", size = 2) +
            geom_point(data = UM_object()$loc.max, color = "red", size = 2) +
            geom_point(data = sel_min, color = "blue", size = 4) +
            geom_point(data = sel_max, color = "red", size = 4) +
            geom_hline(yintercept = lower_bound, color = "red", linetype = "dashed") +
            geom_hline(yintercept = upper_bound, color = "blue", linetype = "dashed")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
