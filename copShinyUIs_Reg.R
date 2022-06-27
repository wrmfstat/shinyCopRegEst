# Definindo uma Interface de usuário (UI, em inglês) para a
# aplicação de histogramas em todos os cenários, por classe
# de regressão e correlação:

ui_25_ph = fluidPage(
  theme = bs_theme(bootswatch="darkly"),
  
  titlePanel("RB for PH Models (n=500, tau=0.25)"),
  fluidRow(column(width=3,
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
    checkboxGroupInput(
      inputId="coefs", label="Model Parameters",
      # choices=coef_ph,
      selected=coef_ph, inline=T,
      choiceNames=coef_ph_uc, choiceValues=coef_ph),
  ),
  
  column(width = 9, mainPanel(plotOutput("rbPlots"))),
  
  titlePanel(
    "MC Results for PH Models (n=500, tau=0.25)"),
  fluidRow(column(width = 9,
    mainPanel(dataTableOutput("tabResults"))))
  )
)

ui_50_ph = fluidPage(
  theme = bs_theme(bootswatch="darkly"),
  
  titlePanel("RB for PH Models (n=500, tau=0.5)"),
  fluidRow(column(width=3,
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
    checkboxGroupInput(
      inputId="coefs", label="Model Parameters",
      # choices=coef_ph,
      selected=coef_ph, inline=T,
      choiceNames=coef_ph_uc, choiceValues=coef_ph),
  ),
  
  column(width = 9, mainPanel(plotOutput("rbPlots"))),
  
  titlePanel(
    "MC Results for PH Models (n=500, tau=0.5)"),
  fluidRow(column(width = 9,
    mainPanel(dataTableOutput("tabResults"))))
  )
)

ui_75_ph = fluidPage(
  theme = bs_theme(bootswatch="darkly"),
  
  titlePanel("RB for PH Models (n=500, tau=0.75)"),
  fluidRow(column(width=3,
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
    checkboxGroupInput(
      inputId="coefs", label="Model Parameters",
      # choices=coef_ph,
      selected=coef_ph, inline=T,
      choiceNames=coef_ph_uc, choiceValues=coef_ph),
  ),
  
  column(width = 9, mainPanel(plotOutput("rbPlots"))),
  
  titlePanel(
    "MC Results for PH Models (n=500, tau=0.75)"),
  fluidRow(column(width = 9,
    mainPanel(dataTableOutput("tabResults"))))
  )
)

ui_25_po = fluidPage(
  theme = bs_theme(bootswatch="darkly"),
  
  titlePanel("RB for PO Models (n=500, tau=0.25)"),
  fluidRow(column(width=3,
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
    checkboxGroupInput(
      inputId="coefs", label="Model Parameters",
      # choices=coef_po,
      selected=coef_po, inline=T,
      choiceNames=coef_po_uc, choiceValues=coef_po),
  ),
  
  column(width = 9, mainPanel(plotOutput("rbPlots"))),
  
  titlePanel(
    "MC Results for PO Models (n=500, tau=0.25)"),
  fluidRow(column(width = 9,
    mainPanel(dataTableOutput("tabResults"))))
  )
)

ui_50_po = fluidPage(
  theme = bs_theme(bootswatch="darkly"),
  
  titlePanel("RB for PO Models (n=500, tau=0.50)"),
  fluidRow(column(width=3,
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
    checkboxGroupInput(
      inputId="coefs", label="Model Parameters",
      # choices=coef_po,
      selected=coef_po, inline=T,
      choiceNames=coef_po_uc, choiceValues=coef_po),
  ),
  
  column(width = 9, mainPanel(plotOutput("rbPlots"))),
  
  titlePanel(
    "MC Results for PO Models (n=500, tau=0.50)"),
  fluidRow(column(width = 9,
    mainPanel(dataTableOutput("tabResults"))))
  )
)

ui_75_po = fluidPage(
  theme = bs_theme(bootswatch="darkly"),
  
  titlePanel("RB for PO Models (n=500, tau=0.75)"),
  fluidRow(column(width=3,
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
    checkboxGroupInput(
      inputId="coefs", label="Model Parameters",
      # choices=coef_po,
      selected=coef_po, inline=T,
      choiceNames=coef_po_uc, choiceValues=coef_po),
  ),
  
  column(width = 9, mainPanel(plotOutput("rbPlots"))),
  
  titlePanel(
    "MC Results for PO Models (n=500, tau=0.75)"),
  fluidRow(column(width = 9,
    mainPanel(dataTableOutput("tabResults"))))
  )
)

ui_25_yp = fluidPage(
  theme = bs_theme(bootswatch="darkly"),
  
  titlePanel("RB for YP Models (n=500, tau=0.25)"),
  fluidRow(column(width=3,
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
    checkboxGroupInput(
      inputId="coefs", label="Model Parameters",
      # choices=coef_yp,
      selected=coef_yp, inline=T,
      choiceNames=coef_yp_uc, choiceValues=coef_yp),
  ),
  
  column(width = 9, mainPanel(plotOutput("rbPlots"))),
  
  titlePanel(
    "MC Results for YP Models (n=500, tau=0.25)"),
  fluidRow(column(width = 9,
    mainPanel(dataTableOutput("tabResults"))))
  )
)

ui_50_yp = fluidPage(
  theme = bs_theme(bootswatch="darkly"),
  
  titlePanel("RB for YP Models (n=500, tau=0.50)"),
  fluidRow(column(width=3,
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
    checkboxGroupInput(
      inputId="coefs", label="Model Parameters",
      # choices=coef_yp,
      selected=coef_yp, inline=T,
      choiceNames=coef_yp_uc, choiceValues=coef_yp),
  ),
  
  column(width = 9, mainPanel(plotOutput("rbPlots"))),
  
  titlePanel(
    "MC Results for YP Models (n=500, tau=0.50)"),
  fluidRow(column(width = 9,
    mainPanel(dataTableOutput("tabResults"))))
  )
)

ui_75_yp = fluidPage(
  theme = bs_theme(bootswatch="darkly"),
  
  titlePanel("RB for YP Models (n=500, tau=0.75)"),
  fluidRow(column(width=3,
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
    checkboxGroupInput(
      inputId="coefs", label="Model Parameters",
      # choices=coef_yp,
      selected=coef_yp, inline=T,
      choiceNames=coef_yp_uc, choiceValues=coef_yp),
  ),
  
  column(width = 9, mainPanel(plotOutput("rbPlots"))),
  
  titlePanel(
    "MC Results for YP Models (n=500, tau=0.75)"),
  fluidRow(column(width = 9,
    mainPanel(dataTableOutput("tabResults"))))
  )
)