


library(shiny)
library(tidyverse)


ui <- fluidPage(
  headerPanel("Exemplo..."),
  fluidRow(
    column(4,
           selectInput(
             inputId = "nivel",
             choices = c("brazil", "regions", "states", "cities"),
             label = "Nível"),
    ),
    
    column(8,
           plotOutput("grafico")
    ),
  )
  
)

server <- function(input, output, session){
  
  url_brazil <- "https://github.com/dest-ufmg/covid19repo/blob/master/data/brazil.rds?raw=true"
  url_regions <- "https://github.com/dest-ufmg/covid19repo/blob/master/data/regions.rds?raw=true"
  url_states <- "https://github.com/dest-ufmg/covid19repo/blob/master/data/states.rds?raw=true"
  url_cities <- "https://github.com/dest-ufmg/covid19repo/blob/master/data/cities.rds?raw=true"
  
  data <- reactive({
    switch(input$nivel,
                    brazil = try(readRDS(url(url_brazil)), TRUE),
                    regions = try(readRDS(url(url_regions)), TRUE),
                    states = try(readRDS(url(url_states)), TRUE),
                    cities = try(readRDS(url(url_cities)), TRUE)
    )
  })
  
  output$grafico <- renderPlot({
    
    if(input$nivel == "cities"){
      df <- data() %>%
        filter(city == "Belo Horizonte")
    }else if(input$nivel == "states"){
      df <- data() %>%
        filter(state == "MG")
    }else if(input$nivel == "regions"){
      df <- data() %>%
        filter(region == "Southeast")
    }else{
      df <- data()
    }
    
    ggplot(df, aes(x = date, y = newDeaths)) +
      geom_point() +
      geom_path()
  })
}

# Para rodar o aplicativo:
shinyApp(ui = ui, server = server)
  


