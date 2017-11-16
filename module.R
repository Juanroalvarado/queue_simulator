

displayMu <- function(id){
  ns <- NS(id)
  fluidRow(
    box(
      h3("Lambda"),
      textOutput(ns("origin_lambda")),
      width = 4
    ),
    box(
      h3("Mu"),
      textOutput(ns("origin_mu")),
      width = 4
    ),
    box(
      h3("Mu2"),
      textOutput(ns("origin_mu2")),
      width = 4
    )
  )
}




displayInput <- function(id){
  ns <- NS(id)
  
  fluidRow(
    box(
      h3("Promedio"),
      h5("Unidades en linea de espera"),
      textOutput(ns("waiting_units")),
      h5("Unidades en el sistema"),
      textOutput(ns("avg_units")),
      width = 4
    ),
    box(
      h3("Tiempos"),
      h5("Tiempo promedio que la unidad pasa en la linea de espera"),
      textOutput(ns("avg_waiting_time")),
      h5("Tiempo que la unidad pasa en el sistema"),
      textOutput(ns("time_in_system")),
      width = 4
    ),
    box(
      h3("Probabilidad"),
      h5("Que no tenga que esperar"),
      textOutput(ns("no_wait")),
      h5("Probabilidad que no haya unidades"),
      textOutput(ns("no_units")),
      width = 4
    )
  )
}



generateMu <- function(input, output, session, go, mu, lambda, mu2){
  observeEvent(go(), {
    output$origin_mu <- renderText({
      mu()
    })
    output$origin_mu2 <- renderText({
      mu2()
    })
    output$origin_lambda <- renderText({
      lambda()
    })
    
  })
  
  
}

formulae <- function(input, output, session, go, mu, lambda){
  
  observeEvent(go(), {
    
    output$no_units <- renderText({
      1 - (lambda()/mu())
    })
    
    unit_wait <- reactive({
      (lambda()^2)/(mu() * (mu()-lambda()))
    })
    
    output$waiting_units <- reactive({
      unit_wait()
    })
    
    output$avg_units <- renderText({
      unit_wait() + (lambda()/mu())
    })
    
    avg_time_wait <- reactive({
      unit_wait()/lambda()
    })
    
    output$avg_waiting_time <- renderText({
      avg_time_wait()
    })
    
    output$time_in_system <- renderText({
      avg_time_wait() + (1/mu())
    }) 
    
    output$no_wait <- renderText({
      lambda()/mu()
    })
    
    
  })
  
}