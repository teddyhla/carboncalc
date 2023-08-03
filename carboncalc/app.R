# Carbon cost of POCUS courses
# This is a Shiny web application.
#
# Conceived by A Wong, M Zawadka and T Hla
# see github repo for details 

# DEPENDENCIES 
library(shiny)
library(ggplot2)
library(bslib)
reactlog::reactlog_enable()

#custom function for generating reactive ui output based on 1,2,3 vars 
uvfunc <- function(id, text, total){
        renderUI({
                numericInput(id,
                             label = text,
                             min = 0,
                             max = total, 
                             # max = input$attnd,
                             step = 1,
                             value = 1
                             
                )
        })
}

sumtravel <- function(x, y, shape, scale, constant){
        vals <- rgamma(n = (x+y), shape = shape, scale = scale)
        vals <- vals * constant * 2
        ans <- sum(vals)
        ans
}

# Define UI for application that draws a histogram
ui <- fluidPage(
        # theme = bslib::bs_theme(bootswatch = "sandstone")   (if theme desires)     

    # Application title
    br(),
    titlePanel("Local vs. non-local events: an interactive carbon cost calculator"),
    
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
                            sidebarPanel( width=2,
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
                                        h5("Select variables for Event-1:"),
                                         uiOutput("uiv3"),
                                         uiOutput("uiv4"),
                                         uiOutput("uiv5"),
                                        hr(),
                                        h5("Select variables for Event-2:"),
                                         uiOutput("uiv6"),
                                         uiOutput("uiv7"),
                                         uiOutput("uiv8")
                                         
                            ),
                            
                            # Show output plots 
                            mainPanel(
                                    fluidRow(
                                        column(6,
                                                plotOutput("carboncostPlot")
                                        ),
                                        column(6,
                                               title = "test",
                                                plotOutput("tmap")
                                        )
                                    ),
                                    br(),
                                    fluidRow(
                                            textOutput("txtans")
                                    )
                            )
                    )
            ),
            tabPanel("User Guide & Assumptions",
                     br(),
                     fluidRow(
                             column(12,
                                    h3("User Guide"),
                                    p("Default settings are set at 30 attendees with 5 faculty members travelling over a mean distance of 75km for local course and 1000 km for international course"),
                                    p("Manipulating the variables will reset the graphs and will be redrawn in real time.")
                                    )
                     ),# may be a card
                     hr(),
                     fluidRow(
                             column(12,
                                    h3("Assumptions"),
                                    h4("Duration of course"),
                                    p("We set a default value of event as 2 days requiring 2 overnight hotel stay."),
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
        
        attn <- reactive(ifelse(input$attnd == 0, 0, input$attnd))
        fac <- reactive(ifelse(input$fac == 0, 0, input$fac))
        e1attn_intl <- reactive(ifelse(input$p1_attnd == 0, 0,input$p1_attnd))
        e1attn_home <- reactive(
                 attn() - e1attn_intl()
        )
        e1fac_intl <- reactive(ifelse(input$f1_attnd == 0, 0, input$f1_attnd))
        e1fac_home <- reactive(
                 fac() - e1fac_intl()
        )
        #here we calculte carbon cost of accommodation which is number of rooms * event dur * cost
        e1carbon_hotel <- reactive(
                input$e1accom * input$duration * 10.4
        )
        
        e1distloc <- reactive(
                sumtravel(
                        x = e1attn_home(), y = e1fac_home(), shape = 10, scale = 5, 
                        constant = 0.03549)
                 #here we multiply by constant of local travels and return journey 2
        )
         
        e1distintl <- reactive(
                sumtravel(
                        x = input$p1_attnd, y = input$fac_attnd, shape = 1500, scale = 0.75,
                        constant = 0.14062)
                 #here we multiply by constant of nonlocal travels and return journey 2
        )
        
        daf <- reactive({
                breakdown <- c("local travel","non-local travel","hotel stay","home") 
                carbon_values <- c(distloc(), distintl(), carbon_hotel(), 0)
                df <- data.frame(breakdown,carbon_values)
                df$breakdown <- as.factor(df$breakdown)
                df$perc <- round((df$carbon_values/sum(df$carbon_values)*100),2)
                df
         })
         
        df1 <- reactive({
                 df <- data.frame(
                         breakdown = c("local travel", "non-local travel"),
                         carbon_values= c(sum(distloc()),sum(distintl()))
                 )
                 df
         })
        
        output$carboncostPlot <- renderPlot({
                ggplot(df1(), aes(x = breakdown, y= carbon_values, fill = breakdown))+
                        geom_col() +
                        scale_fill_brewer(palette = "Set1")+
                        labs(
                                title = "Carbon cost of travel",
                                x = "Type of travel",
                                y = "kilograms of Carbondioxide equivalent"
                        )+
                        theme_minimal()+
                        theme(
                                legend.position = "none",
                                plot.title = element_text(size = 16),
                                axis.title = element_text(size = 14),
                                axis.text.x = element_text(size = 14),
                                axis.text.y = element_text(size=14)
                                
                              )
        })
        output$tmap <- renderPlot({
                ggplot(daf(), aes(x = reorder(breakdown,-perc),y= perc, fill = breakdown)) +
                        geom_col() + 
                        scale_fill_brewer(palette = "Set1")+
                        coord_flip() +
                        labs(
                                title = "Contribution of activities",
                                x = "Activity",
                                y = "Percentage of total carbon cost"
                        ) +
                        theme_minimal() + 
                        theme(
                                legend.position = "none",
                                plot.title = element_text(size = 16),
                                axis.title = element_text(size = 14),
                                axis.text.x = element_text(size = 14),
                                axis.text.y = element_text(size=14)
                                
                              )
        })
        output$txtans <- renderText({
                s <- round(sum(daf()$carbon_values),2)
                sprintf("Total carbon cost of this event is %s kilograms of carbon dioxide equivalent. \n
                        Average UK household monthly emission is 91.7 kilograms of carbondioxide equivalent.", s )
        })

}

# Run the application 
shinyApp(ui = ui, server = server)
