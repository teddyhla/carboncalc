#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Conceived by A Wong, M Zawadka and T Hla
# see github repo for details 

# DEPENDENCIES 
library(shiny)



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("lumen"),

    # Application title
    titlePanel("Carbon cost of Local vs. International POCUS courses"),
    
    #horizontal panel
    hr(),
    
    br(),
    tabsetPanel(
            tabPanel("Simulations",
                     br(),
                    sidebarLayout(
                            sidebarPanel("Please select variables", width=2,
                                         br(),
                                         br(),
                                         radioButtons("defset","Settings:", choices= c(
                                                 "Local" = "loc",
                                                 "International" = "intl"
                                         )),
                                         br(),
                                         numericInput("attnd",
                                                     "Variable 1: Number of attendees:",
                                                     min = 1,
                                                     max = 200,
                                                     step = 1,
                                                     value = 30),
                                         numericInput("fac",
                                                     "Variable 2: Number of faculty:",
                                                     min = 1,
                                                     max = 20,
                                                     value = 5),
                                         uiOutput("uiv3"),
                                         uiOutput("uiv4"),
                                         uiOutput("uiv5"),
                                         numericInput("mu_loc","Variable 6: Mean distance for local in km",
                                                      value = 50, min = 5, max = 100,step = 5),
                                         numericInput("mu_int","Variable 7: Mean distance for international in km",
                                                      value = 500, min = 150, max = 1500,step = 50)
                                         
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                    tableOutput('redf'),
                                    plotOutput("distPlot")
                            )
                    )
            ),
            tabPanel("User Guide & Assumptions",
                     fluidRow(
                             column(12,
                                    "test")
                     ),# may be a card
                     fluidRow(
                             column(4,
                                    "test2")
                     ) # may be a card
                     )
    )

    # Sidebar with a slider input for number of bins 
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
                attn <- reactive(input$attnd)
                fac <- reactive(input$fac)
                muloc <- reactive(input$mu_loc)
                muint <- reactive(input$mu_int)
                perc_attn_intl <- reactive(input$p_attnd)
                perc_fac_intl <- reactive(input$fac_attnd)
                perc_hotel <- reactive(input$accom)
                
        # create reactive dataframe
        redf <- function(x, y, facint, attint){
                d1 <- data.frame(
                        id = (1:x),
                        type = rep("attn", x),
                        travel = c(
                                rep("intl",round(attint * x)),
                                rep("home",round((100- attint) * x))
                        )
                )
                d2 <- data.frame(
                        id = (1:y),
                        type = rep("fac", y),
                        travel = c(
                                rep("intl",round(facint * y)),
                                rep("home",round((100-facint) * y))
                        )
                )
                data <- rbind(d1,d2)
                data    
        }
        df <- reactive(redf(x=attn(),y=fac(),facint=perc_fac_intl(),attint=perc_attn_intl()))
        # reactive slider selection
        output$uiv3 <- renderUI({
                if (is.null(input$defset)) 
                       return()
                switch(input$defset,
                       "loc" = sliderInput("p_attnd",
                                           "Variable 3: Percentage of international attendees",
                                           min = 0,
                                           max = 100,
                                           step = 20,
                                           value = 0),
                       "intl" = sliderInput("p_attnd",
                                            "Variable 3: Percentage of international attendees",
                                            min = 0,
                                            max = 100,
                                            step = 20,
                                            value = 100)
                               )
        })
        output$uiv4 <- renderUI({
                if (is.null(input$defset)) 
                        return()
                switch(input$defset,
                       "loc" = sliderInput("fac_attnd",
                                           "Variable 4: Percentage of international faculty",
                                           min = 0,
                                           max = 100,
                                           step = 20,
                                           value = 0),
                       "intl" = sliderInput("fac_attnd",
                                            "Variable 4: Percentage of international faculty",
                                            min = 0,
                                            max = 100,
                                            step = 20,
                                            value = 100)
                        
                )
        })
        output$uiv5 <- renderUI({
                if (is.null(input$defset)) 
                        return()
                switch(input$defset,
                       "loc" = sliderInput("accom",
                                           "Variable 5: Percentage staying in hotels",
                                           min = 0,
                                           max = 100,
                                           step = 20,
                                           value = 0),
                       "intl" = sliderInput("accom",
                                            "Variable 5: Percentage staying in hotels",
                                            min = 0,
                                            max = 100,
                                            step = 20,
                                            value = 100),
                        
                )
        })
        output$redf <- renderTable({
                df()
                })   

    
}

# Run the application 
shinyApp(ui = ui, server = server)
