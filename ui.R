#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyTime)
source("module.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Queue Simulator Subway"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      box(
        numericInput("lambda","Lambda input",0),
        numericInput("lambda_sd","Lambda SD",0),
        width = 12
      ),
      box(
        numericInput("mu","Mu input",0),
        numericInput("mu_sd","Mu SD",0),
        width = 6
      ),
      box(
        numericInput("mu2","Mu2 input",0),
        numericInput("mu_sd2","Mu2 SD",0),
        width = 6
      ),
     numericInput("time_init","Tiempo Inicial",120),
     numericInput("time_unit","Unidad de Tiempo",30),
     numericInput("prepper","Numero de preparadores",2),
     numericInput("cashier","Numero de cajas",1),
     actionButton("go","Correr Simulacion")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h1("Original"),
      displayMu("original"),
      h2("De cola a caja"),
      displayInput("cola_caja"),
      h2("De caja a Salida"),
      displayInput("caja_salida"),
      
      h1("Simulado"),
      displayMu("simulado"),
      h2("De cola a caja sim"),
      displayInput("cola_caja_simulado"),
      h2("De caja a Salida sim"),
      displayInput("caja_salida_simulado"),
      
      h1("Diferencias"),
      h2("Cambios Porcentuales"),
      box(
        textOutput("cambio_lambda"),
        textOutput("cambio_mu1"),
        textOutput("cambio_mu2"),
        width = 12
      ),
      box(
        plotOutput("server_plot"),
        width = 6
      ),
      box(
        plotOutput("cashier_plot"),
        width = 6
      )
    )
  )
))
