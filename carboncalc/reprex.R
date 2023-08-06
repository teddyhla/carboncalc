library(shiny)
reactlog::reactlog_enable()

ui <- fluidPage("reprex app",
                sidebarPanel(
                        numericInput("var1",
                                     "Select total number of people in group:",
                                     min = 2,
                                     max = 700,
                                     step = 1,
                                     value = 2),
                        uiOutput("uivar2"),
                ),
                mainPanel(
                        verbatimTextOutput("result"),
                        hr(),
                        dataTableOutput("result2")
                )
                )

server <- function(input,output) {
        output$uivar2 <- renderUI({
                numericInput("var2", 
                            label ="total number of children", 
                            min = 1, max = input$var1,step = 1, value = 0)
                })
        
        df <- reactive({
                #old line code
                #a <- input$var1
                #b <- input$var2
                
                #new line code
                a <- ifelse(is.null(input$var1),0,input$var1)
                b <- ifelse(is.null(input$var2),0,input$var2)
                total <- a
                adults <- a - b 
                children <- b
                df <- data.frame(total,adults,children)
        })
        
        output$result <- renderPrint(df())
        
        output$result2 <- renderDataTable(df())
        
}

shinyApp(ui = ui, server = server)