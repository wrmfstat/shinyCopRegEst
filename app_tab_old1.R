# First, make a tabset to include all thesis results:

mainPanel(
  tabsetPanel(
    tabPanel("Regression Estimates",
      headerPanel("RB and MC Results for Survival Copula Models"),
      fluidRow(
        column(3, selectInput(
          inputId="reg", choices=c("PH", "PO", "YP"),
          label="Generated/Fitted Regression Class", selected="PH"),
        ),
        column(3, selectInput(
          inputId="cor", choices=c(0.25, 0.5, 0.75),
          label="Correlation", selected = 0.25),
        ),
        column(3, selectInput(
          inputId="gcops",
          choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
          label="Generated Copula", selected="AMH"),
        ),
        column(3, selectInput(
          inputId="fcops",
          choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
          label="Fitted Copula", selected="AMH"),
        ),
        
        # Inserting the boxplots for the chosen input:
        titlePanel(
          "Relative Bias for Regression Parameter Estimates"),
        fluidRow(
          column(width=2,
                 # checkboxGroupInput(
                 #   inputId="gcops", label="Generated Copula",
                 #   choices=gcop, selected=gcop, inline=T),
                 checkboxGroupInput(
                   inputId="gbsls", label="Generated Baseline",
                   choices=gbsl, selected="W", inline=T),
                 # checkboxGroupInput(
                 #   inputId="fcops", label="Fitted Copula",
                 #   choices=fcop, selected=fcop, inline=T),
                 checkboxGroupInput(
                   inputId="fbsls", label="Fitted Baseline",
                   choices=fbsl, selected=fbsl, inline=T),
                 # checkboxGroupInput(
                 #   inputId="coefs", label="Model Parameters",
                 #   selected=coef, inline=T, choiceNames=coef_uc,
                 #   choiceValues=coef),
          ),
          column(width=8, mainPanel(plotOutput("rbPlots"))),
          
          # Summary tables for the chosen input:
          titlePanel("Monte Carlo Results"),
          fluidRow(column(width=8, offset=1, mainPanel(
            dataTableOutput("tabResults"))))
        )
      )
    ),
    
    tabPanel("AIC Criteria Values",
             verbatimTextOutput("aic")),
    
    tabPanel("Correlation Estimates",
             verbatimTextOutput("tau")),
    
    tabPanel("Likelihood Ratio Tests",
             verbatimTextOutput("lrt")),
    
    tabPanel("Crossing Time Estimates",
             verbatimTextOutput("cte")),
  )
)

# Loading the user interface to apply boxplots and tables:

ui <- fluidPage(
  # Defining the theme:
  theme = bs_theme(bootswatch="darkly"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "results",
        "RB and MC Results for Survival Copula Models",
        c("Regression Estimates", "AIC Criteria Values",
          "Correlation Estimates", "Likelihood Ratio Tests",
          "Crossing Time Estimates"), 1)),
    mainPanel(
      tabsetPanel(
        id = "hidden_tabs",
        # Hide the tab values.
        # Can only switch tabs by using `updateTabsetPanel()`
        type = "hidden",
        tabPanelBody("panel1", "Panel 1 content"),
        tabPanelBody("panel2", "Panel 2 content"),
        tabPanelBody("panel3", "Panel 3 content")
      )
    )
  )
)

ui = fluidPage(
  # Defining the theme:
  theme = bs_theme(bootswatch="darkly"),
  
  # Dividing results for each sidebar:
  tabsetPanel(
    tabPanel("RB and MC Results for Survival Copula Models",
             sidebarLayout(fluidRow(
               column(3, selectInput(
                 inputId="reg", choices=c("PH", "PO", "YP"),
                 label="Generated/Fitted Regression Class",
                 selected="PH"),
               ),
               column(3, selectInput(
                 inputId="cor", choices=c(0.25, 0.5, 0.75),
                 label="Correlation", selected = 0.25),
               ),
               column(3, selectInput(
                 inputId="gcops",
                 choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
                 label="Generated Copula", selected="AMH"),
               ),
               column(3, selectInput(
                 inputId="fcops",
                 choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
                 label="Fitted Copula", selected="AMH"),
               ),
               
               # Inserting the boxplots for the chosen input:
               titlePanel(
                 "Relative Bias for Regression Parameter Estimates"),
               fluidRow(column(width=2,
                               # checkboxGroupInput(
                               #   inputId="gcops", label="Generated Copula",
                               #   choices=gcop, selected=gcop, inline=T),
                               checkboxGroupInput(
                                 inputId="gbsls", label="Generated Baseline",
                                 choices=gbsl, selected="W", inline=T),
                               # checkboxGroupInput(
                               #   inputId="fcops", label="Fitted Copula",
                               #   choices=fcop, selected=fcop, inline=T),
                               checkboxGroupInput(
                                 inputId="fbsls", label="Fitted Baseline",
                                 choices=fbsl, selected=fbsl, inline=T),
                               # checkboxGroupInput(
                               #   inputId="coefs",
                               #   label="Model Parameters",
                               #   selected=coef, inline=T,
                               #   choiceNames=coef_uc,
                               #   choiceValues=coef),
               ),
               column(width=8, mainPanel(plotOutput("rbPlots"))),
               
               # Summary tables for the chosen input:
               titlePanel("Monte Carlo Results"),
               fluidRow(column(width=8, offset=1,
                               mainPanel(dataTableOutput("tabResults"))))
               )),
               
             ),
             tabPanel("RB and MC Results for Survival Copula Models"
                      
             )
    )
    
    # Creating a box for the combination choice:
    # headerPanel("RB and MC Results for Survival Copula Models"),
    # fluidRow(
    #   column(3, selectInput(
    #     inputId="reg", choices=c("PH", "PO", "YP"),
    #     label="Generated/Fitted Regression Class", selected="PH"),
    #   ),
    #   column(3, selectInput(
    #     inputId="cor", choices=c(0.25, 0.5, 0.75),
    #     label="Correlation", selected = 0.25),
    #   ),
    #   column(3, selectInput(
    #     inputId="gcops",
    #     choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    #     label="Generated Copula", selected="AMH"),
    #   ),
    #   column(3, selectInput(
    #     inputId="fcops",
    #     choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    #     label="Fitted Copula", selected="AMH"),
    #   ),
    
    # Inserting the boxplots for the chosen input:
    # titlePanel("Relative Bias for Regression Parameter Estimates"),
    # fluidRow(column(width=2,
    #                 # checkboxGroupInput(
    #                 #   inputId="gcops", label="Generated Copula",
    #                 #   choices=gcop, selected=gcop, inline=T),
    #                 checkboxGroupInput(
    #                   inputId="gbsls", label="Generated Baseline",
    #                   choices=gbsl, selected="W", inline=T),
    #                 # checkboxGroupInput(
    #                 #   inputId="fcops", label="Fitted Copula",
    #                 #   choices=fcop, selected=fcop, inline=T),
    #                 checkboxGroupInput(
    #                   inputId="fbsls", label="Fitted Baseline",
    #                   choices=fbsl, selected=fbsl, inline=T),
    #                 # checkboxGroupInput(
    #                 #   inputId="coefs",
    #                 #   label="Model Parameters",
    #                 #   selected=coef, inline=T,
    #                 #   choiceNames=coef_uc,
    #                 #   choiceValues=coef),
    # ),
    # column(width=8, mainPanel(plotOutput("rbPlots"))),
    # 
    # # Summary tables for the chosen input:
    # titlePanel("Monte Carlo Results"),
    # fluidRow(column(width=8, offset=1,
    #                 mainPanel(dataTableOutput("tabResults"))))
    # )
  )
)

####################################################################
# Example 1:

library(shiny)

# Show a tabset that includes a plot, summary, and
# table view of the generated distribution
mainPanel(
  tabsetPanel(
    tabPanel("Plot", plotOutput("plot")),
    tabPanel("Summary", verbatimTextOutput("summary")),
    tabPanel("Table", tableOutput("table"))
  )
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("controller", "Controller", 1:3, 1)
    ),
    mainPanel(
      tabsetPanel(
        id = "hidden_tabs",
        # Hide the tab values.
        # Can only switch tabs by using `updateTabsetPanel()`
        type = "hidden",
        tabPanelBody("panel1", "Panel 1 content"),
        tabPanelBody("panel2", "Panel 2 content"),
        tabPanelBody("panel3", "Panel 3 content")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(session, "hidden_tabs",
                      selected = paste0("panel", input$controller))
  })
}

if (interactive()) {
  shinyApp(ui, server)
}