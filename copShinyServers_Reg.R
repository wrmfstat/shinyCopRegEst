# Definindo servidores separados por classe de regressão, e
# por correlações:

server_25_ph = function(input, output){
  output$rbPlots = renderPlot({
    r_25_ph %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% input$coefs) %>%
      ggplot(aes(x=Parameter, y=RB)) +
      geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      # facet_grid(F.Copula ~ G.Copula) +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        # "psi_11" = expression(paste(psi[11])),
        # "psi_12" = expression(paste(psi[12])),
        # "psi_21" = expression(paste(psi[21])),
        # "psi_22" = expression(paste(psi[22])))) +
        # expression(paste(iota[11])),
        # expression(paste(iota[21])),
        # expression(paste(iota[12])),
        # expression(paste(iota[22])))) +
        expression(paste(beta[11])),
        expression(paste(beta[21])),
        expression(paste(beta[12])),
        expression(paste(beta[22])))) +
      theme(legend.position="bottom")
  }, width = 1000, height = 450)
  
  output$tabResults = renderDataTable(
    tr_25_ph %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% coef_ph_uc),
    options=list(pageLength=12, lengthMenu=c(12, 24, 120)))
}

server_50_ph = function(input, output){
  output$rbPlots = renderPlot({
    r_50_ph %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% input$coefs) %>%
      ggplot(aes(x=Parameter, y=RB)) +
      geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      # facet_grid(F.Copula ~ G.Copula) +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        # "psi_11" = expression(paste(psi[11])),
        # "psi_12" = expression(paste(psi[12])),
        # "psi_21" = expression(paste(psi[21])),
        # "psi_22" = expression(paste(psi[22])))) +
        # expression(paste(iota[11])),
        # expression(paste(iota[21])),
        # expression(paste(iota[12])),
        # expression(paste(iota[22])))) +
        expression(paste(beta[11])),
        expression(paste(beta[21])),
        expression(paste(beta[12])),
        expression(paste(beta[22])))) +
      theme(legend.position="bottom")
  }, width = 1000, height = 450)
  
  output$tabResults = renderDataTable(
    tr_50_ph %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% coef_ph_uc),
    options=list(pageLength=12, lengthMenu=c(12, 24, 120)))
}

server_75_ph = function(input, output){
  output$rbPlots = renderPlot({
    r_75_ph %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% input$coefs) %>%
      ggplot(aes(x=Parameter, y=RB)) +
      geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      # facet_grid(F.Copula ~ G.Copula) +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        # "psi_11" = expression(paste(psi[11])),
        # "psi_12" = expression(paste(psi[12])),
        # "psi_21" = expression(paste(psi[21])),
        # "psi_22" = expression(paste(psi[22])))) +
        # expression(paste(iota[11])),
        # expression(paste(iota[21])),
        # expression(paste(iota[12])),
        # expression(paste(iota[22])))) +
        expression(paste(beta[11])),
        expression(paste(beta[21])),
        expression(paste(beta[12])),
        expression(paste(beta[22])))) +
      theme(legend.position="bottom")
  }, width = 1000, height = 450)
  
  output$tabResults = renderDataTable(
    tr_75_ph %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% coef_ph_uc),
    options=list(pageLength=12, lengthMenu=c(12, 24, 120)))
}

server_25_po = function(input, output){
  output$rbPlots = renderPlot({
    r_25_po %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% input$coefs) %>%
      ggplot(aes(x=Parameter, y=RB)) +
      geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      # facet_grid(F.Copula ~ G.Copula) +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        # "psi_11" = expression(paste(psi[11])),
        # "psi_12" = expression(paste(psi[12])),
        # "psi_21" = expression(paste(psi[21])),
        # "psi_22" = expression(paste(psi[22])))) +
        # expression(paste(iota[11])),
        # expression(paste(iota[21])),
        # expression(paste(iota[12])),
        # expression(paste(iota[22])))) +
        expression(paste(beta[11])),
        expression(paste(beta[21])),
        expression(paste(beta[12])),
        expression(paste(beta[22])))) +
      theme(legend.position="bottom")
  }, width = 1000, height = 450)
  
  output$tabResults = renderDataTable(
    tr_25_po %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% coef_po_uc),
    options=list(pageLength=12, lengthMenu=c(12, 24, 120)))
}

server_50_po = function(input, output){
  output$rbPlots = renderPlot({
    r_50_po %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% input$coefs) %>%
      ggplot(aes(x=Parameter, y=RB)) +
      geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      # facet_grid(F.Copula ~ G.Copula) +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        # "psi_11" = expression(paste(psi[11])),
        # "psi_12" = expression(paste(psi[12])),
        # "psi_21" = expression(paste(psi[21])),
        # "psi_22" = expression(paste(psi[22])))) +
        # expression(paste(iota[11])),
        # expression(paste(iota[21])),
        # expression(paste(iota[12])),
        # expression(paste(iota[22])))) +
        expression(paste(beta[11])),
        expression(paste(beta[21])),
        expression(paste(beta[12])),
        expression(paste(beta[22])))) +
      theme(legend.position="bottom")
  }, width = 1000, height = 450)
  
  output$tabResults = renderDataTable(
    tr_50_po %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% coef_po_uc),
    options=list(pageLength=12, lengthMenu=c(12, 24, 120)))
}

server_75_po = function(input, output){
  output$rbPlots = renderPlot({
    r_75_po %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% input$coefs) %>%
      ggplot(aes(x=Parameter, y=RB)) +
      geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      # facet_grid(F.Copula ~ G.Copula) +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        # "psi_11" = expression(paste(psi[11])),
        # "psi_12" = expression(paste(psi[12])),
        # "psi_21" = expression(paste(psi[21])),
        # "psi_22" = expression(paste(psi[22])))) +
        # expression(paste(iota[11])),
        # expression(paste(iota[21])),
        # expression(paste(iota[12])),
        # expression(paste(iota[22])))) +
        expression(paste(beta[11])),
        expression(paste(beta[21])),
        expression(paste(beta[12])),
        expression(paste(beta[22])))) +
      theme(legend.position="bottom")
  }, width = 1000, height = 450)
  
  output$tabResults = renderDataTable(
    tr_75_po %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% coef_po_uc),
    options=list(pageLength=12, lengthMenu=c(12, 24, 120)))
}

server_25_yp = function(input, output){
  output$rbPlots = renderPlot({
    r_25_yp %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% input$coefs) %>%
      ggplot(aes(x=Parameter, y=RB)) +
      geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      # facet_grid(F.Copula ~ G.Copula) +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        # "phi_11" = expression(paste(phi[11])),
        # "phi_12" = expression(paste(phi[12])),
        # "phi_21" = expression(paste(phi[21])),
        # "phi_22" = expression(paste(phi[22])),
        # "psi_11" = expression(paste(psi[11])),
        # "psi_12" = expression(paste(psi[12])),
        # "psi_21" = expression(paste(psi[21])),
        # "psi_22" = expression(paste(psi[22])))) +
        # expression(paste(iota[11])),
        # expression(paste(iota[21])),
        # expression(paste(iota[12])),
        # expression(paste(iota[22])),
        # expression(paste(upsilon[11])),
        # expression(paste(upsilon[21])),
        # expression(paste(upsilon[12])),
        # expression(paste(upsilon[22])))) +
        expression(paste(beta[11]^"(S)")),
        expression(paste(beta[21]^"(S)")),
        expression(paste(beta[12]^"(S)")),
        expression(paste(beta[22]^"(S)")),
        expression(paste(beta[11]^"(L)")),
        expression(paste(beta[21]^"(L)")),
        expression(paste(beta[12]^"(L)")),
        expression(paste(beta[22]^"(L)")))) +
      theme(legend.position="bottom")
  }, width = 1450, height = 450)
  
  output$tabResults = renderDataTable(
    tr_25_yp %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% coef_yp_uc),
    options=list(pageLength=24, lengthMenu=c(24, 48, 240)))
}

server_50_yp = function(input, output){
  output$rbPlots = renderPlot({
    r_50_yp %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% input$coefs) %>%
      ggplot(aes(x=Parameter, y=RB)) +
      geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      # facet_grid(F.Copula ~ G.Copula) +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        # "phi_11" = expression(paste(phi[11])),
        # "phi_12" = expression(paste(phi[12])),
        # "phi_21" = expression(paste(phi[21])),
        # "phi_22" = expression(paste(phi[22])),
        # "psi_11" = expression(paste(psi[11])),
        # "psi_12" = expression(paste(psi[12])),
        # "psi_21" = expression(paste(psi[21])),
        # "psi_22" = expression(paste(psi[22])))) +
        # expression(paste(iota[11])),
        # expression(paste(iota[21])),
        # expression(paste(iota[12])),
        # expression(paste(iota[22])),
        # expression(paste(upsilon[11])),
        # expression(paste(upsilon[21])),
        # expression(paste(upsilon[12])),
        # expression(paste(upsilon[22])))) +
        expression(paste(beta[11]^"(S)")),
        expression(paste(beta[21]^"(S)")),
        expression(paste(beta[12]^"(S)")),
        expression(paste(beta[22]^"(S)")),
        expression(paste(beta[11]^"(L)")),
        expression(paste(beta[21]^"(L)")),
        expression(paste(beta[12]^"(L)")),
        expression(paste(beta[22]^"(L)")))) +
      theme(legend.position="bottom")
  }, width = 1450, height = 450)
  
  output$tabResults = renderDataTable(
    tr_50_yp %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% coef_yp_uc),
    options=list(pageLength=24, lengthMenu=c(24, 48, 240)))
}

server_75_yp = function(input, output){
  output$rbPlots = renderPlot({
    r_75_yp %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% input$coefs) %>%
      ggplot(aes(x=Parameter, y=RB)) +
      geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color = "black") +
      # facet_grid(F.Copula ~ G.Copula) +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Coefficients", y="Relative Bias (%)") +
      scale_x_discrete(labels = c(
        # "phi_11" = expression(paste(phi[11])),
        # "phi_12" = expression(paste(phi[12])),
        # "phi_21" = expression(paste(phi[21])),
        # "phi_22" = expression(paste(phi[22])),
        # "psi_11" = expression(paste(psi[11])),
        # "psi_12" = expression(paste(psi[12])),
        # "psi_21" = expression(paste(psi[21])),
        # "psi_22" = expression(paste(psi[22])))) +
        # expression(paste(iota[11])),
        # expression(paste(iota[21])),
        # expression(paste(iota[12])),
        # expression(paste(iota[22])),
        # expression(paste(upsilon[11])),
        # expression(paste(upsilon[21])),
        # expression(paste(upsilon[12])),
        # expression(paste(upsilon[22])))) +
        expression(paste(beta[11]^"(S)")),
        expression(paste(beta[21]^"(S)")),
        expression(paste(beta[12]^"(S)")),
        expression(paste(beta[22]^"(S)")),
        expression(paste(beta[11]^"(L)")),
        expression(paste(beta[21]^"(L)")),
        expression(paste(beta[12]^"(L)")),
        expression(paste(beta[22]^"(L)")))) +
      theme(legend.position="bottom")
  }, width = 1450, height = 450)
  
  output$tabResults = renderDataTable(
    tr_75_yp %>% filter(
      G.Copula %in% input$gcops,
      G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops,
      F.Baseline %in% input$fbsls,
      Parameter %in% coef_yp_uc),
    options=list(pageLength=24, lengthMenu=c(24, 48, 240)))
}