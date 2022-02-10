# Defining a single server:

server = function(input, output, session){
  # Reading online data (make the project public later!):
  # url_PH_25 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_ph025.rds"
  # url_PO_25 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_po025.rds"
  # url_YP_25 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_yp025.rds"
  # url_PH_50 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_ph050.rds"
  # url_PO_50 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_po050.rds"
  # url_YP_50 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_yp050.rds"
  # url_PH_75 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_ph075.rds"
  # url_PO_75 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_po075.rds"
  # url_YP_75 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_yp075.rds"
  
  # Choosing results' data from a given input:
  bigdata = reactive({
    switch(input$comb,
           # If reading online:
           # PH_25 = try(readRDS(url(url_PH_25)), TRUE),
           # PO_25 = try(readRDS(url(url_PO_25)), TRUE),
           # YP_25 = try(readRDS(url(url_YP_25)), TRUE),
           # PH_50 = try(readRDS(url(url_PH_50)), TRUE),
           # PO_50 = try(readRDS(url(url_PO_50)), TRUE),
           # YP_50 = try(readRDS(url(url_YP_50)), TRUE),
           # PH_75 = try(readRDS(url(url_PH_75)), TRUE),
           # PO_75 = try(readRDS(url(url_PO_75)), TRUE),
           # YP_75 = try(readRDS(url(url_YP_75)), TRUE),
           
           # Otherwise, if reading locally in the project:
           PH_25 = readRDS("res_exc_cop/res_reg_ph025.rds"),
           PO_25 = readRDS("res_exc_cop/res_reg_po025.rds"),
           YP_25 = readRDS("res_exc_cop/res_reg_yp025.rds"),
           PH_50 = readRDS("res_exc_cop/res_reg_ph050.rds"),
           PO_50 = readRDS("res_exc_cop/res_reg_po050.rds"),
           YP_50 = readRDS("res_exc_cop/res_reg_yp050.rds"),
           PH_75 = readRDS("res_exc_cop/res_reg_ph075.rds"),
           PO_75 = readRDS("res_exc_cop/res_reg_po075.rds"),
           YP_75 = readRDS("res_exc_cop/res_reg_yp075.rds")
           )
    })
  
  # Choosing summary table data from a given input:
  sumdata = reactive({
    switch(input$comb,
           PH_25 = tr %>% filter(Reg.Class=="PH" & TrueTau==0.25),
           PO_25 = tr %>% filter(Reg.Class=="PO" & TrueTau==0.25),
           YP_25 = tr %>% filter(Reg.Class=="YP" & TrueTau==0.25),
           PH_50 = tr %>% filter(Reg.Class=="PH" & TrueTau==0.50),
           PO_50 = tr %>% filter(Reg.Class=="PO" & TrueTau==0.50),
           YP_50 = tr %>% filter(Reg.Class=="YP" & TrueTau==0.50),
           PH_75 = tr %>% filter(Reg.Class=="PH" & TrueTau==0.75),
           PO_75 = tr %>% filter(Reg.Class=="PO" & TrueTau==0.75),
           YP_75 = tr %>% filter(Reg.Class=="YP" & TrueTau==0.75)
           )
  })
  
  # First, open the output environments!
  
  output$rbPlots = renderPlot({
    # Fixing on another object to allow updates:
    df = bigdata()
    
    # Recoding all parameter names:
    df$Parameter = case_when(
      df$Parameter=="psi[1,1]" & df$Reg.Class!="YP"~"beta[1,1]",
      df$Parameter=="psi[1,2]" & df$Reg.Class!="YP"~"beta[1,2]",
      df$Parameter=="psi[2,1]" & df$Reg.Class!="YP"~"beta[2,1]",
      df$Parameter=="psi[2,2]" & df$Reg.Class!="YP"~"beta[2,2]",
      df$Parameter=="psi[1,1]" & df$Reg.Class=="YP"~"beta^(S)[1,1]",
      df$Parameter=="psi[1,2]" & df$Reg.Class=="YP"~"beta^(S)[1,2]",
      df$Parameter=="psi[2,1]" & df$Reg.Class=="YP"~"beta^(S)[2,1]",
      df$Parameter=="psi[2,2]" & df$Reg.Class=="YP"~"beta^(S)[2,2]",
      df$Parameter=="phi[1,1]" & df$Reg.Class=="YP"~"beta^(L)[1,1]",
      df$Parameter=="phi[1,2]" & df$Reg.Class=="YP"~"beta^(L)[1,2]",
      df$Parameter=="phi[2,1]" & df$Reg.Class=="YP"~"beta^(L)[2,1]",
      df$Parameter=="phi[2,2]" & df$Reg.Class=="YP"~"beta^(L)[2,2]")
    
    # If we want PH or PO models:
    # if(sum(which(df$Reg.Class=="YP"))==0){
    if(input$comb=="PH_25" | input$comb=="PO_25" |
       input$comb=="PH_50" | input$comb=="PO_50" |
       input$comb=="PH_75" | input$comb=="PO_75"){
      # Saving separatedely those new names:
      coef = c("beta[1,1]", "beta[2,1]",
               "beta[1,2]", "beta[2,2]")
      
      # Filtering our data according to selected options:
      df = df %>% filter(
        G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
        F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
        # Parameter %in% input$coefs)
        Parameter %in% coef)
      
      # Plotting the boxplots for the relative bias:
      ggplot(df, aes(x=Parameter, y=RB)) + geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color="black") +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        expression(paste(beta[11])), expression(paste(beta[21])),
        expression(paste(beta[12])), expression(paste(beta[22])))) +
      theme(legend.position="bottom")
    }
    # If we want YP models:
    else{
      # Saving separatedely those new names:
      coef = c("beta^(S)[1,1]", "beta^(S)[2,1]",
               "beta^(S)[1,2]", "beta^(S)[2,2]",
               "beta^(L)[1,1]", "beta^(L)[2,1]",
               "beta^(L)[1,2]", "beta^(L)[2,2]")
      
      # Filtering our data according to selected options:
      df = df %>% filter(
        G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
        F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
        # Parameter %in% input$coefs)
        Parameter %in% coef)
      
      # Plotting the boxplots for the relative bias:
      ggplot(df, aes(x=Parameter, y=RB)) + geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        expression(paste(beta[11]^"(S)")),
        expression(paste(beta[21]^"(S)")),
        expression(paste(beta[12]^"(S)")),
        expression(paste(beta[22]^"(S)")),
        expression(paste(beta[11]^"(L)")),
        expression(paste(beta[21]^"(L)")),
        expression(paste(beta[12]^"(L)")),
        expression(paste(beta[22]^"(L)")))) +
      theme(legend.position="bottom")
    }
  },
  # width = 1000, height = 450)
  width = 900, height = 400)

  output$tabResults = renderDataTable({
    # If we want PH or PO models:
    # if(sum(which(df$Reg.Class=="YP"))==0){
    if(input$comb=="PH_25" | input$comb=="PO_25" |
       input$comb=="PH_50" | input$comb=="PO_50" |
       input$comb=="PH_75" | input$comb=="PO_75"){
      # Saving separatedely those new names:
      coef_uc = c("\u03B2\U2081\U2081", "\u03B2\U2082\U2081",
                  "\u03B2\U2081\U2082", "\u03B2\U2082\U2082")
      # Fixing on another object to allow updates:
      dft = sumdata() %>% filter(
        G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
        F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
        Parameter %in% coef_uc)
    }
    else{
      # Saving separatedely those new names:
      coef_uc = c("\u03B2^(S)\U2081\U2081","\u03B2^(S)\U2082\U2081",
                  "\u03B2^(S)\U2081\U2082","\u03B2^(S)\U2082\U2082",
                  "\u03B2^(L)\U2081\U2081","\u03B2^(L)\U2082\U2081",
                  "\u03B2^(L)\U2081\U2082","\u03B2^(L)\U2082\U2082")
      
      # Summary tables:
      dft = sumdata() %>% filter(
        G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
        F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
        Parameter %in% coef_uc)
    }
  },
  # options=list(pageLength=12, lengthMenu=c(12, 24, 120))
  options=list(pageLength=24, lengthMenu=c(24, 48, 240)))
}
