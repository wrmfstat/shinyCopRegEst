####################################################################

# Cleaning the working environment:
rm(list=ls(all=TRUE))

# Loading the required packages:
library(dplyr)
library(shiny)

# 1st application test for reading .rds data from websites: 

# Defining a user interface, just to illustrate our application:

ui = fluidPage(
  actionButton(inputId="brazil", label="Brazil"),
  actionButton(inputId="region", label="Southeast"),
  actionButton(inputId="states", label="MG"),
  
  titlePanel("Accumulated Deaths"),
  column(width=9, mainPanel(plotOutput("Histograms"))),
)

# Defining a single server which will allow multiple inputs from the
# user when running the application:

server = function(input, output, session) {
  # Note 1: "reactive" function did not work as separated blocks!
  
  # data = reactive({
  #   input$brazil
  #   url = "https://github.com/dest-ufmg/covid19repo/blob/master/data/brazil.rds?raw=true"
  #   data = try(readRDS(url(url)), TRUE)
  # })
  # 
  # data = reactive({
  #   input$regions
  #   url = "https://github.com/dest-ufmg/covid19repo/blob/master/data/regions.rds?raw=true"
  #   data = try(readRDS(url(url)), TRUE)
  # })
  
  # Note 2: "reactive" function did not work as a united block with all inputs!
  
  # data = reactive({
  #   input$Brazil
  #   url = "https://github.com/dest-ufmg/covid19repo/blob/master/data/brazil.rds?raw=true"
  #   data = try(readRDS(url(url)), TRUE)
  #   
  #   input$Regions
  #   url = "https://github.com/dest-ufmg/covid19repo/blob/master/data/regions.rds?raw=true"
  #   data = try(readRDS(url(url)), TRUE)
  # })
  
  # Note 3: Apparently, the function "observeEvent" did work!
  
  observeEvent(input$brazil,
   {url = "https://github.com/dest-ufmg/covid19repo/blob/master/data/regions.rds?raw=true";
    data = try(readRDS(url(url)), TRUE)}
  )
  observeEvent(input$region,
   {url = "https://github.com/dest-ufmg/covid19repo/blob/master/data/regions.rds?raw=true";
    data = try(readRDS(url(url)), TRUE);
    data = filter(data, region=="Southeast")}
  )
  observeEvent(input$states,
   {url = "https://github.com/dest-ufmg/covid19repo/blob/master/data/states.rds?raw=true";
    data = try(readRDS(url(url)), TRUE);
    data = filter(data, state=="MG")}
  )
  
  output$Histograms = renderPlot({
    data %>% ggplot(aes(accumDeaths)) + geom_histogram() + labs(x="")
  })
}

# Running the application, but allowing multiple inputs:

shinyApp(ui=ui, server=server)

###############################################################################################

url_brazil <- "https://github.com/dest-ufmg/covid19repo/blob/master/data/brazil.rds?raw=true"
url <- "http://www.est.ufmg.br/~fndemarqui/walmir/cities.rds"

dados <- try(readRDS(url(url_brazil)), TRUE)
data <- try(readRDS(url(url)), TRUE)

library(shiny)    
runGitHub(repo = "moedas",
          username = "fndemarqui")

tb <- reactive({
  input$atualizar
  tb <- tibble(
    prop = replicate(1e3, mean(rbinom(n = input$n,  size = 1, prob = input$p)))
  ) %>%
    mutate(
      z = (prop - input$p)/sqrt(input$p*(1-input$p)/input$n)
    )
})

url_brazil <- "https://github.com/dest-ufmg/covid19repo/blob/master/data/brazil.rds?raw=true"
url_regions <- "https://github.com/dest-ufmg/covid19repo/blob/master/data/regions.rds?raw=true"
url_states <- "https://github.com/dest-ufmg/covid19repo/blob/master/data/states.rds?raw=true"
url_cities <- "https://github.com/dest-ufmg/covid19repo/blob/master/data/cities.rds?raw=true"