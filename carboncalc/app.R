# Carbon cost of POCUS courses
# This is a Shiny web application.
#
# Conceived by A Wong, M Zawadka and T Hla
# see github repo for details 

# DEPENDENCIES 
library(shiny)
library(plotly)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "sandstone"),

    # Application title
    titlePanel("Local vs. International POCUS courses: an interactive simulated Carbon Cost Calculator"),
    
    #horizontal panel
    p("Explore the carbon cost of POCU courses by manipulating the seven different variables below. Please see 'User Guide & Assumptions' for details."),
    p("Conceived and developed by",a("Dr Adrian Wong", href= "https://twitter.com/avkwong?lang=en" ), a("Dr Mateusz Zawadka", href= "https://twitter.com/m_zawadka?lang=en"), "and", a("Dr Teddy Tun Win Hla",href= "https://twitter.com/teddyhla?lang=en-GB")
            
    ),
    
    
    #main side bar panel
    br(),
    tabsetPanel(
            tabPanel("Simulations",
                     br(),
                    sidebarLayout(
                            sidebarPanel("Please select variables individually", width=2,
                                         br(),
                                         br(),
                                         radioButtons("defset","Default Settings:", choices= c(
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
                                    plotlyOutput("carboncostPlot"),
                                    verbatimTextOutput("test2"),
                                    DT::dataTableOutput('redf'),
                            )
                    )
            ),
            tabPanel("User Guide & Assumptions",
                     fluidRow(
                             column(12,
                                    h3("User Guide"),
                                    p("Default settings are set at 30 attendees with 5 faculty members travelling over a mean distance of 50km for local course and 500 km for international course"),
                                    p("Manipulating the variables will reset the graph and new graph drawn and displayed in real time.")
                                    )
                     ),# may be a card
                     hr(),
                     fluidRow(
                             column(12,
                                    h3("Assumptions"),
                                    h4("Assumption 1"),
                                    p("We assumed that POCUS courses are run over two days. Thus, if not staying at home, attendees and faculty will require 2-night hotel stay."),
                                    p("We assumed that all international attendees are not sharing rooms in a hotel and not staying locally with friends and family."),
                                    h4("Assumption 2"),
                                    p("We have deliberately not calculated the carbon cost of moving equipments and venue set up. This is because a POCUS course is likely to require venue and equipment irregardless of locality."),
                                    h4("Assumption 3"),
                                    p("We assumed that all international travels are via flights, travelling in 'economy class'. We assumed that all local travels are via 'national rail' without added car/ taxi journeys."),
                                    h4("Assumption 4"),
                                    p("Carbon foot print is more accurately subdivided into carbon dioxide, methane, and nitrous oxide levels. For parsimony, we have reported a total equivalent carbon dioxide as a single value."),
                                    h4("Assumption 5"),
                                    p("We assumed that all travel distance to venue comprises of a return journey and follows a normal distribution."),
                                    h4("Reference"),
                                    p("For our calculation, we have used", a("UK Government green house gas conversions", href= "https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2022"))
                                    )
                     ) # may be a card
                     ),
            tabPanel("About",
                     fluidRow(
                             column(12,
                                    h4("Version"),
                                    p("0.1"),
                                    h4("License"),
                                    p("GPL-3"),
                                    h4("Authors"),
                                    p("If there are any queries or bugs or feedback in using this app, please contact", a("Dr Teddy Tun Win Hla",href= "https://twitter.com/teddyhla?lang=en-GB")),
                                    p("If you would like to embed this app in your website, you may use the following code but we would be grateful if you could notify us."),
                                    h4("Cite this app as"),
                                    p("Please use the following to cite in publications:"),
                                    
                                    )
                     ),
                     hr(),
                     fluidRow(
                             
                     )
                    
            )
    )

    # Sidebar with a slider input for number of bins 
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        # here we will create reactive variables
        # key logics - for see readme.md
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
                        rnorm(n= (attn_home() + fac_home()) , mean= input$mu_loc , sd = 1)
                )
                distintl <- reactive(
                        rnorm(n= (input$p_attnd + input$fac_attnd), mean= input$mu_int , sd = 10 )
                )
        # here we will create a reactive dataframe.
         df <- reactive({
                 #first an id is assigned based on number of attendees and faculty
                 # then they are classed as faculty or attendees
                 # then their mode of travel is assigned
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
                #then their state of accommodation is assigned.
                df$carbon_accomo <- ifelse(df$accommodation == "hotel",20.8,0)
                #then dataframe is sorted based on mode of travel
                df<-df[order(df$travel),]
                # then we assigned distances computed
                df$dist <- ifelse(df$travel == "local",distloc(),distintl())
                # see read me for rationale of this
                df$carbon_travel <- ifelse(df$travel == "local",0.03549,0.14062)
                df$total_carbon <- (df$carbon_travel * 2 * df$dist) + df$carbon_accomo
                #dont forget to return a reactive dataframe back
                #change to factors for
                df$type <- as.factor(df$type)
                df$accommodation <- as.factor(df$accommodation)
                df$travel <- as.factor(df$travel)
                df
         })
         
        # reactive slider selection based on default setting of local vs. international
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
                                           min = (attn_intl()+fac_intl()),
                                           max = (input$fac + input$attnd),
                                           step = 1,
                                           value = (attn_intl()+fac_intl())),
                       "intl" = sliderInput("accom",
                                            "Variable 5: Number staying in hotels",
                                            min = (attn_intl()+fac_intl()),
                                            max = (input$fac + input$attnd),
                                            step = 1,
                                            value = (input$fac + input$attnd)),
                        
                )
        })
        output$redf <- DT::renderDataTable({
                df()
                }) 
        output$test2 <- renderPrint({
                summary(df())
        })
        output$carboncostPlot <- renderPlotly({
                plot_ly(df(), x = ~travel, y= ~total_carbon  )
        })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
