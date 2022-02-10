# Defining a single user interface:

ui = fluidPage(
  # Defining the theme:
  theme = bs_theme(bootswatch="darkly"),
  
  # Creating buttons for each combination of class and correlation:
  # actionButton(inputId="ph025", label="PH, 0.25"),
  # actionButton(inputId="po025", label="PO, 0.25"),
  # actionButton(inputId="yp025", label="YP, 0.25"),
  # actionButton(inputId="ph050", label="PH, 0.5"),
  # actionButton(inputId="po050", label="PO, 0.5"),
  # actionButton(inputId="yp050", label="YP, 0.5"),
  # actionButton(inputId="ph075", label="PH, 0.75"),
  # actionButton(inputId="po075", label="PO, 0.75"),
  # actionButton(inputId="yp075", label="YP, 0.75"),
  
  # Or instead, creating a box for the combination choice:
  headerPanel(
    "RB and MC Results for Survival Copula Models"),
  fluidRow(
    column(3, selectInput(
    inputId = "comb",
    choices = c("PH_25", "PO_25", "YP_25",
                "PH_50", "PO_50", "YP_50",
                "PH_75", "PO_75", "YP_75"),
    label = "Regression Class x Correlation"),
    ),
           
    # Inserting the boxplots for the chosen input:
    titlePanel("Relative Bias, Given Class and Correlation"),
    fluidRow(column(width=2,
      checkboxGroupInput(
        inputId="gcops", label="Generated Copula",
        choices=gcop, selected=gcop, inline=T),
      checkboxGroupInput(
        inputId="gbsls", label="Generated Baseline",
        choices=gbsl, selected=gbsl, inline=T),
      checkboxGroupInput(
        inputId="fcops", label="Fitted Copula",
        choices=fcop, selected=fcop, inline=T),
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
      column(width=9, mainPanel(plotOutput("rbPlots"))),
                    
      # Summary tables for the chosen input:
      titlePanel(
        "Monte Carlo Results, Given Class and Correlation"),
      fluidRow(column(width=9, mainPanel(
        dataTableOutput("tabResults"))))
      )
    )
  )