#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Conceived by A Wong, M Zawadka and T Hla
# see github repo for details 

# DEPENDENCIES 
library(shiny)
library(dplyr)
reactiveConsole(TRUE)


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
                                    DT::dataTableOutput('redf'),
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
                attn_intl <- reactive(input$p_attnd)
                attn_home <- reactive(
                        attn() - attn_intl()
                )
                fac_intl <- reactive(input$fac_attnd)
                fac_home <- reactive(
                        fac() - fac_intl()
                )
                perc_hotel <- reactive(input$accom)
                perc_home <- reactive(
                        (attn() + fac()) - perc_hotel()
                )
                distloc <- reactive(
                        rnorm((attn_home() + fac_home()) , mean= input$mu_loc , sd = 1)
                )
                distintl <- reactive(
                        rnorm((attn_intl() + fac_intl()), mean= input$mu_intl , sd = 10 )
                )
         df <- reactive({
                 
                df <- data.frame(
                        id = (1:(attn() + fac())),
                        type = c(rep("attn",attn()), rep("fac",fac())),
                        travel = c(
                                rep("intl", attn_intl()),
                                rep("local", attn_home()),
                                rep("intl", fac_intl()),
                                rep("local", fac_home())
                        ),
                        accommodation = c(
                                rep("hotel",perc_hotel()),
                                rep("home",perc_home())
                        )
                        #if else doesnt work because it is not assigned yet!
                )
         })
        # create reactive dataframe
        
        # reactive slider selection
        output$uiv3 <- renderUI({
                if (is.null(input$defset)) 
                       return()
                switch(input$defset,
                       "loc" = sliderInput("p_attnd",
                                           "Variable 3: Number of international attendees",
                                           min = 0,
                                           max = input$attnd,
                                           step = 1,
                                           value = 0),
                       "intl" = sliderInput("p_attnd",
                                            "Variable 3: Number of international attendees",
                                            min = 0,
                                            max = input$attnd,
                                            step = 1,
                                            value = input$attnd)
                               )
        })
        output$uiv4 <- renderUI({
                if (is.null(input$defset)) 
                        return()
                switch(input$defset,
                       "loc" = sliderInput("fac_attnd",
                                           "Variable 4: Number of international faculty",
                                           min = 0,
                                           max = input$fac,
                                           step = 1,
                                           value = 0),
                       "intl" = sliderInput("fac_attnd",
                                            "Variable 4: Number of international faculty",
                                            min = 0,
                                            max = input$fac,
                                            step = 1,
                                            value = input$fac)
                        
                )
        })
        output$uiv5 <- renderUI({
                if (is.null(input$defset)) 
                        return()
                switch(input$defset,
                       "loc" = sliderInput("accom",
                                           "Variable 5: Number staying in hotels",
                                           min = 0,
                                           max = (input$fac + input$attnd),
                                           step = 1,
                                           value = 0),
                       "intl" = sliderInput("accom",
                                            "Variable 5: Number staying in hotels",
                                            min = 0,
                                            max = (input$fac + input$attnd),
                                            step = 1,
                                            value = (input$fac + input$attnd)),
                        
                )
        })
        output$redf <- DT::renderDataTable({
                df()
                })   

    
}

# Run the application 
shinyApp(ui = ui, server = server)
