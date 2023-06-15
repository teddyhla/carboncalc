library(shiny)
library(bslib)
library(plotly)
library(reactlog)

reactlog_enable()

app <- system.file("carboncalc/carboncalc/app")
runApp(app)

# once app has closed, display reactlog from shiny
shiny::reactlogShow()
