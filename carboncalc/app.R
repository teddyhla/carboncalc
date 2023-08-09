# Carbon cost of Events - aShiny web application.
#
# Conceived by A Wong, M Zawadka and T Hla
# see github repo for details 

# DEPENDENCIES 
library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
source("utils.R")

# UI elements

costcard <- card(
        full_screen = TRUE,
        card_header(
                class = "bg-dark",
                h5("Carbon cost of travel")
        ),
        card_body(
                plotlyOutput("carboncostPlot")
        )
)

tmap <- card(
        full_screen = TRUE,
        card_header(
                class = "bg-dark",
                h5("Contribution of activities")
        ),
        card_body(
                plotlyOutput("tmap")
        )
)

sidebar_acc <- accordion(
        open = c("Select global variables:","Variables for Option A:","Variables for Option B:"),
        accordion_panel(
                "Select global variables:",icon = icon("globe"),
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
                             value = 2)
        ),
        accordion_panel(
                "Variables for Option A:",icon = icon("a"),
                uiOutput("uiv3"),
                uiOutput("uiv4"),
                uiOutput("uiv5")
        ),
        accordion_panel(
                "Variables for Option B:",icon = icon("b"),
                uiOutput("uiv6"),
                uiOutput("uiv7"),
                uiOutput("uiv8")
        )
)

# UI
ui <- page_navbar(
        fillable_mobile = FALSE,
        title = "Events carbon cost calculator",
        sidebar = sidebar(sidebar_acc),
        nav_panel(title = "Simulations",
                br(),
                layout_columns(costcard,tmap),
                layout_columns(
                        value_box(class = "bg-warning",showcase = icon("a"),title = "Option A Total Carbon Cost",
                                value = textOutput("txt1ans"),
                                p("kilograms of carbondioxide equivalent.")
                        ),
                        value_box(class = "bg-primary",showcase = icon("b"),title = "Option B Total Carbon Cost",
                                value = textOutput("txt2ans"),
                                p("kilograms of carbondioxide equivalent.")
                        ),
                        value_box(class = "bg-info",showcase = icon("car-side"),title = "Driving a 4-seater, deisel car for 100km",
                                value = "12.7",
                                div(HTML("<p>kilograms of carbondioxide equivalent (<em>assuming average efficiency) <sup>[1]</sup></em></p>"))
                        )
                        )
                ),
        nav_panel(title = "User Guide & Assumptions",
                                  page_fillable(
                                          br(),
                                        card(height = "150px",
                                                card_header(h6("User Guide")),
                                                card_body(mot2())
                                        ),
                                        card(
                                                card_header(h6("Assumptions")),
                                                card_body(a1())
                                        )
                                )
                                
                ),
        nav_panel(title = "About",
                        page_fillable(
                                card(height = "200px",
                                     card_header(h6("Development Team")),
                                     card_body(ver1())
                                     
                                ),
                                card(
                                        card_header(h6("References")),
                                        card_body(ref1())
                                        
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
                gen(a= input$attnd,b=input$fac,c=input$p1_attnd,d =input$f1_attnd,e = input$duration,f= input$e1_accom,g = "Option A")
        })
        #make a reactive dataframe for event2
        edf2 <- reactive({
                gen(a= input$attnd, b= input$fac, c=input$p2_attnd, d=input$f2_attnd, e= input$duration,f=input$e2_accom, g="Option B")
        })
        
        daf <- reactive({
                df <- rbind(edf1(),edf2())
                df$breakdown <- as.factor(df$breakdown)
                df$option <- as.factor(df$option)
                df
         })
        
        df_filtered <- reactive({
                daf()[grepl("travel", daf()$breakdown), ]
        })
        
        #output plot1
        output$carboncostPlot <- renderPlotly({
                
                plot1 <-ggplot(df_filtered(), aes(x = breakdown, y= carbon_values, fill = option))+
                              geom_col(position = "dodge") +
                              scale_fill_manual(values = c("#FFBF27","#0081FC"))+
                              labs(
                                      x = "Type of travel",
                                      y = "Carbondioxide equivalent in kg",
                                      fill = NULL
                              )+
                              theme_cc()
                plotly::ggplotly(plot1,tooltip = c("y","text","fill"))
        })
        #output plot2 
        output$tmap <- renderPlotly({
                ab = daf()
                plot2 <-ggplot(ab, aes(x = reorder(breakdown,-perc),y= perc, fill = option)) +
                            geom_col(position = "dodge") + 
                            scale_fill_manual(values = c("#FFBF27","#0081FC"))+
                            coord_flip() +
                            labs(
                                    x = "Activity",
                                    y = "Percentage of total carbon cost",
                                    fill = NULL
                                    
                            ) +
                            theme_cc()
                plotly::ggplotly(plot2,tooltip = c("y","text","fill"))
                
        })
        # output texts
        output$txt1ans <- renderText(txrd(df = edf1))
        output$txt2ans <- renderText(txrd(df = edf2))

}

# Run the application 
shinyApp(ui = ui, server = server)
