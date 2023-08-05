# Carbon cost of Events
# This is a Shiny web application.
#
# Conceived by A Wong, M Zawadka and T Hla
# see github repo for details 

# DEPENDENCIES 
library(shiny)
library(ggplot2)
library(bslib)
library(plotly)
reactlog::reactlog_enable()

#custom function for generating reactive ui output based on 1,2,3 vars 
#uvfunc creates a ui elements for two events, taking arguments id, text and total which is from 
# original set variables
uvfunc <- function(id, text, total){
        renderUI({
                numericInput(id, label = text, min = 0,max = total,step = 1,value = 1)
        })
}

#sumtravel function is a function that takes two values, and generates a gamma distribution
#then it sums all the output and results it as a value.
sumtravel <- function(x, y, shape, scale, constant){
        vals <- rgamma(n = (x+y), shape = shape, scale = scale)
        vals <- vals * constant * 2
        ans <- sum(vals)
        ans
}

#gen function takes 7 arguments and generates a dataframe of variable and it pulls sumtravel function
gen <- function(a, b, c, d, e, f, g){
        # a = total attendee, b = total fac, c = intl attendee , d = intl fac
        # e = duration, f = hotel, g = model type
        model <- g
        breakdown <- c("local travel", "intl travel", "hotel stay","home")
        hotelc <- f * e * 10.4 # dur event * no hotel rooms * unit cost
        # local popn = total attendee - intl attendee & total fac - intl fac
        localc <- sumtravel(x= (a - c), y=(b-d), shape= 10, scale= 5, constant = 0.03549)
        intlc <- sumtravel(x = c,y = d , shape =1500, scale = 0.75, constant = 0.14062)
        carbon_values <- c(localc, intlc, hotelc, 0)
        df <- data.frame(model, breakdown, carbon_values)
        df$perc <- round((df$carbon_values/sum(df$carbon_values)*100),2)
        df
}

# custom function that help generate a text 
txrd <- function(a, df){
        sprintf("Total carbon cost for %s is %s kilograms of carbon dioxide equivalent",a, round(sum(df()$carbon_values),2))
}

#lets define a custom ggplot theme
theme_cc <- function(){
        font <- "Arial"
        theme_minimal() %+replace%
                theme(
                        #panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        plot.title = element_text(
                                family = font,
                                size = 16,
                                face = 'bold',
                                hjust =0 ,
                                vjust = 2,
                        ),
                        axis.title = element_text(
                                family = font,
                                size = 16,
                        ),
                        axis.text.x = element_text(family = font,size = 12,vjust = 2),
                        axis.text.y = element_text(family = font, size=12),
                        legend.text = element_text(family = font, size =11 )
                )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
        theme = bslib::bs_theme(),   #(if theme desires)     

    # Application title
    br(),
    titlePanel("Event carbon cost calculator"),
    
    #horizontal panel
    p("Explore the carbon cost of events by manipulating variables below. Please see 'User Guide & Assumptions' for details."),
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
                                        h5("Select variables for Model 1:"),
                                         uiOutput("uiv3"),
                                         uiOutput("uiv4"),
                                         uiOutput("uiv5"),
                                        hr(),
                                        h5("Select variables for Model 2:"),
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
                                            tags$li("Driving 4-seater car with average efficiency diesel fuel for 100km approximately consumes 39 kilograms of carbondioxide equivalent.")
                                    ),
                                    fluidRow(
                                            verbatimTextOutput("test"),
                                            
                                    )
                            )
                    )
            ),
            tabPanel("User Guide & Assumptions",
                     br(),
                     fluidRow(
                             column(12,
                                    h3("User Guide"),
                                    p("First, set global variables(total attendees, total faculty, and event duration). Then set different combination of international and local attendee / faculty for comparison of event 1 and event 2 carbon costs.Default is set at 30 attendees with 5 faculty members travelling over a mean distance of 75km for local course and 1000 km for international course"),
                                    p("Manipulating the variables will reset the graphs and will be redrawn in real time.")
                                    )
                     ),# may be a card
                     hr(),
                     fluidRow(
                             column(12,
                                    h3("Assumptions"),
                                    h4("Duration of course"),
                                    p("We assumed that all international attendees are not sharing rooms in a hotel and not staying locally with friends and family."),
                                    h4("Carbon cost of venue & equipment"),
                                    p("We have not calculated the carbon cost of catering, equipments transport, waste and venue set up. This is because these costs are likely to be similar for local / non-local events."),
                                    h4("Type of transports"),
                                    p("Based on EU travel data, we assumed that all international travels are via flights, travelling in 'economy class' as direct flights."),
                                    p("Based on UK National Travel Survey[1], surface rail remains the most common mode of transport for average miles travelled per person per year. Therefore, we assumed that 'surface rail' will be used for local travel. Based on average travel durations in the UK and European Union, for local travel we have assumed a 75km travel distance and for international air travel we have assumed a 1000 km distance."),
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
                     br(),
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

    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        bslib::bs_themer()
        output$uiv3 <- uvfunc(id = "p1_attnd",text = "Number of international attendees", total = input$attnd)
        output$uiv4 <- uvfunc(
                id = "f1_attnd", text = "Number of international faculty", total = input$fac
        )
        output$uiv5 <- uvfunc(
                id = "e1_accom",text = "Number staying in hotels", total = (input$attnd + input$fac + 15)
        )
        
        output$uiv6 <- uvfunc(id = "p2_attnd",text = "Number of international attendees", total = input$attnd)
        output$uiv7 <- uvfunc(
                id = "f2_attnd", text = "Number of international faculty", total = input$fac
        )
        output$uiv8 <- uvfunc(
                id = "e2_accom",text = "Number staying in hotels", total = (input$attnd + input$fac + 15)
        )
       
        # here we will create reactive variables
        # key logics - for see readme.md
        # make a reactive dataframe for event1 
        edf1 <- reactive({
                gen(a= input$attnd,b=input$fac,c=input$p1_attnd,d =input$f1_attnd,e = input$duration,f= input$e1_accom,g = "Model 1")
        })
        #make a reactive dataframe for event2
        edf2 <- reactive({
                gen(a= input$attnd, b= input$fac, c=input$p2_attnd, d=input$f2_attnd, e= input$duration,f=input$e2_accom, g="Model 2")
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
        
        output$carboncostPlot <- renderPlotly({
                
                plot1 <-ggplot(df_filtered(), aes(x = breakdown, y= carbon_values, fill = model))+
                              geom_col(position = "dodge") +
                              scale_fill_brewer(palette = "Set1")+
                              labs(
                                      title = "Carbon cost of travel",
                                      x = "Type of travel",
                                      y = "kilograms of Carbondioxide equivalent"
                              )+
                              theme_cc()
                plotly::ggplotly(plot1,tooltip = c("y","text","fill"))
        })
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
        output$txt1ans <- renderText(txrd(a = "Model 1", df = edf1))
        output$txt2ans <- renderText(txrd(a="Model 2",df = edf2))
        
        output$test <- renderPrint(
                daf()
        )

}

# Run the application 
shinyApp(ui = ui, server = server)
