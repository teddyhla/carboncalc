# Carbon cost of Events - aShiny web application.
#
# Conceived by A Wong, M Zawadka and T Hla
# see github repo for details 

# DEPENDENCIES 
library(shiny)
library(ggplot2)
library(plotly)
#reactlog::reactlog_enable()
source("utils.R")

# UI
ui <- fluidPage(
        # theme = bslib::bs_theme(),

    # App title
    titlePanel("Event carbon cost calculator"),
    p("Explore the carbon cost of events by manipulating variables below."),
    p("Conceived and developed by",a("Dr Adrian Wong,", href= "https://twitter.com/avkwong?lang=en" ), a("Dr Mateusz Zawadka", href= "https://twitter.com/m_zawadka?lang=en"), "and", a("Dr Teddy Tun Win Hla",href= "https://twitter.com/teddyhla?lang=en-GB")
    ),
    
    #main side bar panel
    br(),
    tabsetPanel(
            tabPanel("Simulations",
                     br(),
                    sidebarLayout(
                            sidebarPanel( width=3,
                                         h4("Select global variables:"),
                                         br(),
                                         numericInput("attnd",
                                                     "Total number of attendees:",
                                                     min = 1,
                                                     max = 1000,
                                                     step = 1,
                                                     value = 30),
                                         numericInput("fac",
                                                     "Total number of faculty:",
                                                     min = 1,
                                                     max = 1000,
                                                     value = 5),
                                        numericInput("duration",
                                                     "Duration of event in days:",
                                                     min = 1,
                                                     max = 7,
                                                     step = 1,
                                                     value = 2),
                                        hr(),
                                        h5("Select variables for Model A:"),
                                         uiOutput("uiv3"),
                                         uiOutput("uiv4"),
                                         uiOutput("uiv5"),
                                        hr(),
                                        h5("Select variables for Model B:"),
                                         uiOutput("uiv6"),
                                         uiOutput("uiv7"),
                                         uiOutput("uiv8")
                                         
                            ),
                            
                            # Show output plots 
                            mainPanel(
                                    fluidRow(
                                        column(6,
                                                plotlyOutput("carboncostPlot")
                                        ),
                                        column(6,
                                                plotlyOutput("tmap")
                                        )
                                    ),
                                    br(),
                                    hr(),
                                    tags$ul(
                                            tags$li(h5(textOutput("txt1ans"))),
                                            br(),
                                            tags$li(h5(textOutput("txt2ans"))),
                                            br(),
                                            tags$li(h5("Driving a 4-seater car with average efficiency diesel fuel in EU for 100km approximately consumes 12.7 kilograms of carbondioxide equivalent [1]."))
                                    ),
                                    fluidRow(
                                            verbatimTextOutput("test"),
                                            
                                    )
                            )
                    )
            ),
            tabPanel("User Guide & Assumptions",
                     fluidRow(
                             column(12,
                                    h3("Instructions"),
                                    motxt())
                     ),# may be a card
                     hr(),
                     fluidRow(
                             column(12,
                                    h3("Assumptions"),
                                    a1()
                                    )
                     ) # may be a card
                     ),
            tabPanel("About",
                     br(),
                     fluidRow(
                             column(12,
                                    h4("Version"),
                                    p("1.0"),
                                    h4("License"),
                                    p("GPL-3"),
                                    h4("Authors"),
                                    p(" To direct message the app developer, please contact", a("Dr Teddy Tun Win Hla",href= "https://twitter.com/teddyhla?lang=en-GB")),
                                    h4("Source code"),
                                    p("Source code is available at",a('github repo link',href= "https://github.com/teddyhla/carboncalc/tree/master/carboncalc" )),
                                    h4("Cite this app as"),
                                    cite1()
                                    )
                     ),
                     hr(),
                     fluidRow(
                             column(12,
                                    h4("References"),
                                    ref1()
                                    )
                             
                     )
                    
            )
    )

    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        #bslib::bs_themer()
        #histdata variable is a hack to allow modal display on load.
        histdata <- rnorm(1)
        #modal display output
        observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, {
                showModal(modalDialog(
                        title = "User Instructions",
                        motxt()
                ))
        })
        # dynamic UI display using functions
        output$uiv3 <- uvfunc(id = "p1_attnd",text = "Number of international attendees", total = input$attnd, val = input$attnd)
        output$uiv4 <- uvfunc(
                id = "f1_attnd", text = "Number of international faculty", total = input$fac, val = input$fac
        )
        output$uiv5 <- uvfunc(
                id = "e1_accom",text = "Number staying in hotels", total = (input$attnd + input$fac + 15), val = (input$attnd + input$fac)
        )
        
        output$uiv6 <- uvfunc(id = "p2_attnd",text = "Number of international attendees", total = input$attnd,)
        output$uiv7 <- uvfunc(
                id = "f2_attnd", text = "Number of international faculty", total = input$fac, 
        )
        output$uiv8 <- uvfunc(
                id = "e2_accom",text = "Number staying in hotels", total = (input$attnd + input$fac + 15)
        )
       
        # here we will create reactive variables
        # key logics - for see readme.md
        # make a reactive dataframe for event1 
        edf1 <- reactive({
                gen(a= input$attnd,b=input$fac,c=input$p1_attnd,d =input$f1_attnd,e = input$duration,f= input$e1_accom,g = "Model A")
        })
        #make a reactive dataframe for event2
        edf2 <- reactive({
                gen(a= input$attnd, b= input$fac, c=input$p2_attnd, d=input$f2_attnd, e= input$duration,f=input$e2_accom, g="Model B")
        })
        
        daf <- reactive({
                df <- rbind(edf1(),edf2())
                df$breakdown <- as.factor(df$breakdown)
                df$model <- as.factor(df$model)
                df
         })
        
        df_filtered <- reactive({
                daf()[grepl("travel", daf()$breakdown), ]
        })
        
        #output plot1
        output$carboncostPlot <- renderPlotly({
                
                plot1 <-ggplot(df_filtered(), aes(x = breakdown, y= carbon_values, fill = model))+
                              geom_col(position = "dodge") +
                              scale_fill_brewer(palette = "Set1")+
                              labs(
                                      title = "Carbon cost of travel",
                                      x = "Type of travel",
                                      y = "Carbondioxide equivalent in kg "
                              )+
                              theme_cc()
                plotly::ggplotly(plot1,tooltip = c("y","text","fill"))
        })
        #output plot2 
        output$tmap <- renderPlotly({
                ab = daf()
                plot2 <-ggplot(ab, aes(x = reorder(breakdown,-perc),y= perc, fill = model)) +
                            geom_col(position = "dodge") + 
                            scale_fill_brewer(palette = "Set1")+
                            coord_flip() +
                            labs(
                                    title = "Contribution of activities",
                                    x = "Activity",
                                    y = "Percentage of total carbon cost"
                            ) +
                            theme_cc()
                plotly::ggplotly(plot2,tooltip = c("y","text","fill"))
                
        })
        # output texts
        output$txt1ans <- renderText(txrd(a = "Model A", df = edf1))
        output$txt2ans <- renderText(txrd(a="Model B",df = edf2))
        
        #output for checking logic and testing. to be removed in launch.
        output$test <- renderPrint(
                daf()
        )

}

# Run the application 
shinyApp(ui = ui, server = server)
