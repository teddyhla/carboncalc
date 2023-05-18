#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Conceived by A Wong, M Zawadka and T Hla
# see github repo for details 

# DEPENDENCIES 
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),

    # Application title
    titlePanel("Carbon cost of Local vs. International POCUS courses"),
    
    #horizontal panel
    hr(),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel("Please select variables",
                br(),
                     br(),
            sliderInput("attnd",
                        "Variable 1: Number of attendees:",
                        min = 1,
                        max = 200,
                        value = 30),
                br(),
            sliderInput("fac",
                        "Variable 2: Number of faculty:",
                        min = 1,
                        max = 20,
                        value = 5),
            br(),
            sliderInput("p_attnd",
                        "Variable 3: Percentage of international attendees",
                        min = 0,
                        max = 100,
                        value = 50),
            br(),
            sliderInput("fac_attnd",
                        "Variable 4: Percentage of international faculty",
                        min = 0,
                        max = 100,
                        value = 50),
            numericInput("mu_loc","Variable 5: Mean distance for local in km",
                         value = 50, min = 5, max = 100,step = 5),
            br(),
            numericInput("mu_int","Variable 6: Mean distance for international in km",
                         value = 500, min = 150, max = 1500,step = 50)
                
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
