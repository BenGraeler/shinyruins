#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Climate model explorer"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        
        column(2,
               selectInput("gcm", "GCM", choices = c("CanESM2", "CERFACS-CNRM-CM5")),
               selectInput("rcm", "RCM", choices = c("STARS3", "WETTREG2013")),
               selectInput("rcp", "RCP", choices = c("85", "45")),
               selectInput("var", "Variable", choices = c("T", "Tmin", "Tmax", "Prec", "Rs", "RH", "u2", "vabar", "aP", "EToPM", "EToPM1", "EToHG", "EToSJ", "EToPT", "scPDSIhg", "scPDSIpm")),
               selectInput("dec", "Dekade", choices = c("2000","2010","2020","2030","2040","2050","2060","2070","2080", "2090")),
        ),
        column(8,
               plotOutput("tsPlot"),
               fluidRow(column(9,
                               sliderInput("months", label = "Monate", min = 1, max = 12, value = c(1,12))),
                        column(3, 
                               checkboxInput("reverseMonth", "Invertiere Auswahl"))),
               plotOutput("densityPlot")
        ),
        column(2,
               selectInput("gcm2", "GCM", choices = c("CanESM2", "CERFACS-CNRM-CM5")),
               selectInput("rcm2", "RCM", choices = c("STARS3", "WETTREG2013")),
               selectInput("rcp2", "RCP", choices = c("85", "45")),
               selectInput("var2", "Variable", choices = c("T", "Tmin", "Tmax", "Prec", "Rs", "RH", "u2", "vabar", "aP", "EToPM", "EToPM1", "EToHG", "EToSJ", "EToPT", "scPDSIhg", "scPDSIpm")),
               selectInput("dec2", "Dekade", choices = c("2000","2010","2020","2030","2040","2050","2060","2070","2080", "2090"), selected = "2090"),
        )
    )
))
