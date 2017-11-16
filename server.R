#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
library(simmer)
library(reshape2)
library(dplyr)
library(simmer.plot)
source("module.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  cola_caja <- callModule(formulae, "cola_caja", reactive(input$go), reactive(input$mu), reactive(input$lambda))
  caja_salida <- callModule(formulae, "caja_salida", reactive(input$go), reactive(input$mu2), reactive(input$mu))
  
  original <- callModule(generateMu, "original", reactive(input$go), reactive(input$mu), reactive(input$lambda), reactive(input$mu2))
  
  observeEvent(input$go, {
    
    mu_l <- reactive({
      lambda = input$lambda
      lambda_sd = input$lambda_sd
      mu1 = input$mu
      mu1_sd = input$mu_sd
      mu2 = input$mu2
      mu2_sd = input$mu_sd2
      
      rho = lambda/mu1
      sub.N <- rho/(1-rho)
      
      numero_de_panes = input$prepper
      numero_de_cajas = input$cashier
      
      tiempo_a_correr = input$time_init
      div_tiempo = input$time_unit
      
      looper = tiempo_a_correr/div_tiempo
      
      
      set.seed(1234)
      
      subway <- simmer()
      
      cliente <-
        trajectory("Camino de Cliente") %>%
        log_("Llego") %>%
        
        ## Llega al primer paso de hacer su subway
        set_attribute("start_time_sub", function() {now(subway)}) %>%
        seize("servidor") %>%
        log_(function() {paste("Waited for sub: ", now(subway) - get_attribute(subway, "start_time_sub"))}) %>%
        timeout(function() abs(rnorm(1,mu1,mu1_sd))) %>%
        release("servidor") %>%
        
        ## Llega a caja
        set_attribute("start_time_caja", function() {now(subway)}) %>%
        seize("caja") %>%
        log_(function() {paste("Waited for caja: ", now(subway) - get_attribute(subway, "start_time_caja"))}) %>%
        timeout(function() abs(rnorm(1,mu2,mu2_sd))) %>%
        release("caja") %>%
        set_attribute("salio_sistema", function() {now(subway)}) %>%
        log_(function() {paste("Finished: ", now(subway))})
      
      subway <-
        simmer("subway") %>%
        add_resource("servidor", capacity = numero_de_panes) %>%
        add_resource("caja", capacity = numero_de_cajas) %>%
        add_generator("Cliente", cliente, function() abs(rnorm(1,lambda,lambda_sd)), mon = 2)
      
      subway %>% run(until = tiempo_a_correr)
      
      output$server_plot <- renderPlot({
        plot(subway, "resources", "usage", "servidor", items="system")
      })
      
      output$cashier_plot <- renderPlot({
        plot(subway, "resources", "usage", "caja", items="system")
      })
      
      subway.att <- get_mon_attributes(subway) %>% 
        select(name, key, time) %>%
        dcast(name ~ key, value.var="time") %>%
        mutate(tasa_de_pan = (start_time_caja - start_time_sub), tasa_de_caja = (salio_sistema - start_time_caja))
      
      lower_bound = 0
      upper_bound = div_tiempo
      mu_lambda <- data.frame()
      
      while(upper_bound <= tiempo_a_correr){
        a <- subway.att[subway.att$start_time_sub >= lower_bound & subway.att$start_time_sub < upper_bound ,]
        mu_lambda <- rbind(mu_lambda, 
                           c(nrow(a), mean(a$tasa_de_pan, na.rm = TRUE), mean(a$tasa_de_caja, na.rm = TRUE))
        )
        lower_bound = upper_bound
        upper_bound = upper_bound + div_tiempo
        print(lower_bound)
        print(upper_bound)
      }
      
      colnames(mu_lambda) <- c("Lambda","Mu1","Mu2")
      mu_lambda
      
    })
    
    
    
    mu1_sim <- mean(mu_l()$Mu1, na.rm = TRUE)
    mu2_sim <- mean(mu_l()$Mu2, na.rm = TRUE)
    lambda_sim <- mean(mu_l()$Lambda, na.rm = TRUE)
    
    
    simulado <- callModule(generateMu, "simulado", reactive(input$go), reactive(mu1_sim), reactive(lambda_sim), reactive(mu2_sim))
    cola_caja_simulado <- callModule(formulae, "cola_caja_simulado", reactive(input$go), reactive(mu1_sim), reactive(lambda_sim))
    caja_salida_simulado <- callModule(formulae, "caja_salida_simulado", reactive(input$go), reactive(mu2_sim), reactive(mu1_sim))
    
    output$cambio_lambda <- renderText({
      cambio <- round(((input$lambda - lambda_sim)/input$lambda) * 100, digits = 2)
      paste("Cambio en lambda: ",cambio,"%")
      
    })
    output$cambio_mu1 <- renderText({
      cambio <- round(((input$mu - mu1_sim)/input$mu) * 100, digits = 2)
      paste("Cambio en mu1: ",cambio,"%")
      
    })
    output$cambio_mu2 <- renderText({
      cambio <- round(((input$mu2 - mu2_sim)/input$mu2) * 100, digits = 2)
      paste("Cambio en mu2: ",cambio,"%")
    })
    
    
    
  })
  
  
  
  
  
  
  
})

