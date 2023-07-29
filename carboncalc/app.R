# Carbon cost of POCUS courses
# This is a Shiny web application.
#
# Conceived by A Wong, M Zawadka and T Hla
# see github repo for details 

# DEPENDENCIES 
library(shiny)
#library(shinyBS)
#for adding tooltips
library(plotly)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(bslib)
reactlog::reactlog_enable()

# Define UI for application that draws a histogram
ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "sandstone"),
                

    # Application title
    titlePanel("Local vs. non-local events : an interactive simulated Carbon Cost Calculator"),
    
    #horizontal panel
    p("Explore the carbon cost of courses by manipulating different variables below. Please see 'User Guide & Assumptions' for details."),
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
                                        #br(),
                                        # radioButtons("defset","Default Settings:", choices= c(
                                        #         "Local" = "loc",
                                        #         "International" = "intl"
                                        # )),
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
                                         numericInput("duration",
                                                      "Variable 6: Duration of event in days:",
                                                      min = 1,
                                                      max = 7,
                                                      step = 1,
                                                      value = 2
                                                      )
                                         #numericInput("mu_loc","Variable 6: Mean distance for local in km",
                                          #            value = 100, min = 5, max = 250,step = 5),
                                         #numericInput("mu_int","Variable 7: Mean distance for international in km",
                                          #            value = 1000, min = 500, max = 2500,step = 100)
                                         
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                    fluidRow(
                                        column(6,
                                                plotOutput("carboncostPlot")  
                                        ),
                                        column(6,
                                                plotOutput("tmap")
                                        )
                                    )
                                    #fluidRow(
                                    #        verbatimTextOutput("test2")
                                    #)
                                    # plotlyOutput("carboncostPlot"),
                                     
                                    # DT::dataTableOutput('redf'),
                            )
                    )
            ),
            tabPanel("User Guide & Assumptions",
                     fluidRow(
                             column(12,
                                    h3("User Guide"),
                                    p("Default settings are set at 30 attendees with 5 faculty members travelling over a mean distance of 50km for local course and 1000 km for international course"),
                                    p("Manipulating the variables will reset the graph and new graph drawn and displayed in real time.")
                                    )
                     ),# may be a card
                     hr(),
                     fluidRow(
                             column(12,
                                    h3("Assumptions"),
                                    h4("Duration of course"),
                                    p("We assumed that courses are run over two days and thus default value is 2 but maximum value is 7 days. Thus, if not staying at home, attendees and faculty will require 2-night hotel stay."),
                                    p("We assumed that all international attendees are not sharing rooms in a hotel and not staying locally with friends and family."),
                                    h4("Carbon cost of venue and POCUS equipment"),
                                    p("We have deliberately not calculated the carbon cost of moving equipments and venue set up. This is because an event type will determine equipments and irregardless of locality, events are all likely to require venue and equipment."),
                                    h4("Type of transports"),
                                    p("We assumed that all international travels are via flights, travelling in 'economy class' as direct flights. We assumed that all local travels are via 'surface rail' without added car/ taxi journeys."),
                                    p("Based on UK National Travel Survey[1], surface rail remains the most common mode of transport for average miles travelled per person per year. As a result, for local travel, we assumed that 'surface rail' will be used as most common mode of travel. Based on average travel durations in the UK and European Union, for local travel we have assumed a 75km travel distance and for international air travel we have assumed a 1000 km distance."),
                                    h4("Breakdown of carbon cost"),
                                    p("Carbon foot print is more accurately subdivided into carbon dioxide, methane, and nitrous oxide levels. For parsimony, we have reported a total equivalent carbon dioxide as a single value."),
                                    h4("Typical journey"),
                                    p("We assumed that all travel distance to venue comprises of a return journey and follows a gamma distribution [2].For 'typical' local courses, we have modelled using a mean distance of 75km (approximately 1 hour 15 mins surface rail journey time) as a maximum upperlimit of acceptable commute."),
                                    p("Based on Eurocontrol, an average flight distance travelled in European Union is 981km and as a result, we have used as a 1000 km as default for international flight [3]. "),
                                    h4("References"),
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
                                    h4("Source code"),
                                    p("Source code is available at",a('github repo link',href= "https://github.com/teddyhla/carboncalc/tree/master/carboncalc" )),
                                    h4("Cite this app as"),
                                    p("Please use the following to cite in publications:")
                                    
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
        
        attn <- reactive(ifelse(input$attnd == 0, 0, input$attnd))
        fac <- reactive(ifelse(input$fac == 0, 0, input$fac))
        attn_intl <- reactive(ifelse(input$p_attnd == 0, 0,input$p_attnd))
        attn_home <- reactive(
                 attn() - attn_intl()
        )
        fac_intl <- reactive(ifelse(input$fac_attnd == 0, 0, input$fac_attnd))
        fac_home <- reactive(
                 fac() - fac_intl()
        )
        no_hotel <- reactive(ifelse(input$accom == 0, 0, input$accom))
        no_home <- reactive(
                 (attn() + fac()) - no_hotel()
        )
        dur <- reactive(
                 input$duration
        )
         
        distloc <- reactive(
                 rgamma(
                         n = (attn_home() + fac_home()), shape = 10, scale = 5
                 )
        )
         
        distintl <- reactive(
                 rgamma(
                         n = (input$p_attnd + input$fac_attnd), shape = 1500, scale = 0.75
                 )
        )
        #distloc <- reactive(
        #        rnorm(n= (attn_home() + fac_home()) , mean= input$mu_loc , sd = 1)
        #)
        #distintl <- reactive(
        #        rnorm(n= (input$p_attnd + input$fac_attnd), mean= input$mu_int , sd = 10 )
        #)
        # here w will create a reactive dataframe.
         daf <- reactive({
                 #first an id is assigned based on number of attendees and faculty
                 # then they are classed as faculty or attendees
                 # then their mode of travel is assigned
                df <- data.frame(
                        id = (1:(attn() + fac())),
                        type = c(rep("attendee",times = attn()), rep("faculty",times = fac())),
                        travel = c(
                                 rep("nonlocal", times = attn_intl()),
                                 rep("local", times = attn_home()),
                                 rep("nonlocal",times = fac_intl()),
                                 rep("local", times = fac_home())
                        ),
                         accommodation = c(
                                 rep("hotel",times = no_hotel()),
                                 rep("home",times = no_home())
                         ),
                        eventdur = rep(dur(), times = (attn() + fac()))
                        #if else doesnt work because it is not assigned yet!
                )
                 #then their state of accommodation is assigned.
                 df$carbon_accomo_cost <- ifelse(df$accommodation == "hotel",20.8,0)
                 #then lets multiply carbon accommo total by duration of course
                 df$total_carbon_accomo <- df$carbon_accomo_cost * df$eventdur
                 #then dataframe is sorted based on mode of travel
                 df<-df[order(df$travel),]
                 # then we assigned distances computed
                 df$dist <- ifelse(df$travel == "local",distloc(),distintl())
                 
                 # see read me for rationale of this
                 df$carbon_travel_cost <- ifelse(df$travel == "local",0.03549,0.14062)
                 # now total travel 
                 df$total_carbon_travel <- (df$carbon_travel_cost * 2 * df$dist)
                 df$sum_carbon <- df$total_carbon_travel + df$total_carbon_accomo
                 #dont forget to return a reactive dataframe back
                 #change to factors for
                 df$type <- as.factor(df$type)
                 df$accommodation <- as.factor(df$accommodation)
                 df$travel <- as.factor(df$travel)
                df
         })
         df2 <- reactive({
                d1 <- daf() %>% select("travel", "total_carbon_travel") 
                names(d1) <- c("breakdown","c_carbon")
                d2 <- daf() %>% select("accommodation","total_carbon_accomo")
                names(d2) <- c("breakdown", "c_carbon")
                df2 <- rbind(d1,d2)
                df2$frac <- df2$c_carbon / sum(df2$c_carbon)
                df2$ymax <- cumsum(df2$frac)
                df2$ymin <- c(0,head(df2$ymax, n = -1))
                df2
         })
         
        # reactive slider selection based on default setting of local vs. international
        output$uiv3 <- renderUI({
                
                numericInput("p_attnd",
                        "Variable 3: Number of international attendees",
                        min = 0,
                        max = input$attnd,
                        step = 1,
                        value = 1
                        
                )
                #if (is.null(input$defset)) 
                #       return()
                #switch(input$defset,
                #       "loc" = sliderInput("p_attnd",
                #                           "Variable 3: Number of international attendees",
                #                           min = 0,
                #                           max = input$attnd,
                #                           step = 1,
                #                           value = 0),
                #       "intl" = sliderInput("p_attnd",
                #                            "Variable 3: Number of international attendees",
                #                            min = 0,
                #                            max = input$attnd,
                #                            step = 1,
                #                            value = input$attnd)
                #               )
        })
        output$uiv4 <- renderUI({
                
                numericInput("fac_attnd",
                        "Variable 4: Number of international faculty",
                        min = 0, 
                        max = input$fac,
                        step = 1,
                        value = 1
                        
                )
                #if (is.null(input$defset)) 
                #        return()
                #switch(input$defset,
                #       "loc" = sliderInput("fac_attnd",
                #                           "Variable 4: Number of international faculty",
                #                           min = 0,
                #                           max = input$fac,
                #                           step = 1,
                #                           value = 0),
                #       "intl" = sliderInput("fac_attnd",
                #                            "Variable 4: Number of international faculty",
                #                            min = 0,
                #                            max = input$fac,
                #                            step = 1,
                #                            value = input$fac)
                #        
                #)
        })
        output$uiv5 <- renderUI({
                
                numericInput("accom",
                        "Variable 5: Number staying in hotels",
                        min = 0,
                        max = (input$attnd + input$fac + 15),
                        step = 1,
                        value = 1
                )
                #if (is.null(input$defset)) 
                #        return()
                #switch(input$defset,
                #       "loc" = sliderInput("accom",
                #                           "Variable 5: Number staying in hotels",
                #                           min = (attn_intl()+fac_intl()),
                #                           max = (input$fac + input$attnd),
                #                           step = 1,
                #                           value = (attn_intl()+fac_intl())),
                #       "intl" = sliderInput("accom",
                #                            "Variable 5: Number staying in hotels",
                #                            min = (attn_intl()+fac_intl()),
                #                            max = (input$fac + input$attnd),
                #                            step = 1,
                #                            value = (input$fac + input$attnd)),
                #        
                #)
        })
        output$redf <- DT::renderDataTable({
                daf()
                }) 
        output$test2 <- renderPrint({
                df2()
                #summary(daf())
        })
        output$carboncostPlot <- renderPlot({
                ggplot(daf(), aes(x = travel, y= sum_carbon))+
                        geom_col()
        })
        output$tmap <- renderPlot({
                ggplot(df2(), aes(ymax = ymax, ymin = ymin, xmax =4, xmin = 3, fill = breakdown))+
                        geom_rect() +
                        #geom_label( x = 3.5, aes( y = labelPosition, label = label),size = 6) +
                        scale_fill_brewer(palette = 4) +
                        coord_polar(theta = "y") + 
                        xlim(c(2,4)) +
                        theme_void()
        })
        #output$tmap <- renderPlot({
        #        treemap::treemap(
        #                daf(), #dataframe obj
        #                index = c("travel", "accommodation"),#list of categorical vars
        #                vSize = "sum_carbon",
        #                type = "index",
        #                palette = "Reds",
        #                title = "Fig 2"
        #                
        #        )
        #})
                # plot_ly(
                #         daf(), x = ~travel, y= ~ sum_carbon
                #         ) |>
                #         layout(
                #                 title = list(
                #                         text = "Carbon cost of event",
                #                         font = list(family = "Source Sans pro")
                #                 )
                #                 
                #         )

}

# Run the application 
shinyApp(ui = ui, server = server)
