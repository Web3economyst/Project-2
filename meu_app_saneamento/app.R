library(shiny)
library(ExPanDaR)
library(tidyverse)
library(DT)
library(knitr)
library(kableExtra)
library(htmltools)
library(scales)
library(janitor) 
library(plm)      
library(lmtest) 
library(plotly) 
library(car)
library(stargazer)
library(MASS) 

# ==============================================================================
# FUNÇÃO AUXILIAR CORRIGIDA (Robustez para evitar erros)
# ==============================================================================

cor_pvalue_matrix <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      
      # Verifica se desvio padrão é 0 (variável constante) ou se é tudo NA
      sd_i <- sd(mat[, i], na.rm = TRUE)
      sd_j <- sd(mat[, j], na.rm = TRUE)
      
      if (is.na(sd_i) || is.na(sd_j) || sd_i == 0 || sd_j == 0) {
        p.mat[i, j] <- p.mat[j, i] <- 1 # P-valor 1 se não puder calcular
      } else {
        # Tenta calcular correlação; se der erro, define p=1
        tryCatch({
          tmp <- cor.test(mat[, i], mat[, j], use = "pairwise.complete.obs")
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }, error = function(e) {
          p.mat[i, j] <- p.mat[j, i] <- 1
        })
      }
    }
  }
  
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

# ==============================================================================
# ---- CONFIGURAÇÃO DE DADOS E VARIÁVEIS SNIS ----

GITHUB_CSV_URL <- "https://raw.githubusercontent.com/Web3economyst/Project-2/refs/heads/main/sul_snis_municipio_agua_esgoto.csv"
dependente_esgoto <- "populacao_atendida_esgoto"
dependente_agua   <- "populacao_atendida_agua"

# Nomes seguros
RECEITA_OP_TOTAL_SAFE <- "receita_op_total_safe"
ARRECADACAO_SAFE <- "arrecadacao_total_safe"
CREDITO_RECEBER_SAFE <- "credito_a_receber_safe"
RECEITA_OP_INDIR_SAFE <- "receita_op_indireta_safe"
DESPESA_EXPLORACAO_SAFE <- "despesa_exploracao_safe"
DESPESA_TOTAL_SERVICO_SAFE <- "despesa_total_servico_safe"
DESPESA_FISCAL_SAFE <- "despesa_fiscal_safe"

# Variáveis independentes sugeridas
variaveis_independentes_regressao <- c(
  "receita_operacional_direta_esgoto", DESPESA_EXPLORACAO_SAFE,                
  "investimento_total_prestador", "consumo_eletrico_sistemas_esgoto"   
)

# Lista COMPLETA
variaveis_completas_snis <- unique(c(
  dependente_esgoto, dependente_agua, 
  "receita_operacional_direta_agua", "receita_operacional_direta_esgoto",
  "receita_operacional_direta_agua_exportada", "receita_operacional_direta_esgoto_importado",
  RECEITA_OP_INDIR_SAFE, ARRECADACAO_SAFE, CREDITO_RECEBER_SAFE, 
  "despesa_pessoal", "despesa_produto_quimico", "despesa_energia",
  "despesa_servico_terceiro", DESPESA_EXPLORACAO_SAFE, "despesas_juros_divida",
  DESPESA_TOTAL_SERVICO_SAFE, "despesa_agua_importada", DESPESA_FISCAL_SAFE,
  "despesa_fiscal_nao_computada", "despesa_amortizacao_divida",
  "despesa_esgoto_exportado", RECEITA_OP_TOTAL_SAFE, 
  "investimento_total_prestador", "investimento_total_municipio", "investimento_total_estado",
  "volume_agua_produzido", "consumo_eletrico_sistemas_agua", 
  "consumo_eletrico_sistemas_esgoto"
))

variaveis_obrigatorias <- c("Ano_Ref", "Municipio", "Sigla_UF") 
variaveis_disponiveis_UI <- setdiff(variaveis_completas_snis, variaveis_obrigatorias)

# Assegura que apenas colunas que existem serão mantidas no select
variaveis_a_manter_no_df <- unique(c(variaveis_obrigatorias, variaveis_completas_snis))

# ==============================================================================
# UI
ui <- fluidPage(
  titlePanel("Análise do Painel dos Municípios Gaúchos - Saneamento"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Configuração Geral"),
      selectInput("model_type", "Modelo de Regressão (Painel):",
                  choices = c("OLS", "Efeitos Fixos (FE)", "Efeitos Aleatórios (RE)"),
                  selected = "Efeitos Fixos (FE)"),
      
      selectInput("var_reg_idvs", "Variáveis Independentes (Manual):",
                  choices = variaveis_disponiveis_UI,
                  multiple = TRUE,
                  selected = variaveis_independentes_regressao),
      hr(),
      h4("Visualização"),
      selectInput("var_hist", "Variável Histograma/Extremos:",
                  choices = variaveis_disponiveis_UI, selected = dependente_esgoto),
      
      selectInput("var_group", "Variável Y (Gráfico por Grupo):",
                  choices = variaveis_disponiveis_UI, selected = dependente_esgoto),
      
      selectInput("var_trend", "Variáveis Tendência:",
                  choices = variaveis_disponiveis_UI,
                  multiple = TRUE,
                  selected = c(dependente_esgoto, variaveis_independentes_regressao[1])),
      
      selectInput("var_quantile", "Variável Tendência Quantil:",
                  choices = variaveis_disponiveis_UI, selected = dependente_esgoto),
      
      selectInput("var_scatter_x", "Scatter X:",
                  choices = variaveis_disponiveis_UI, selected = dependente_esgoto),
      
      selectInput("var_scatter_y", "Scatter Y:",
                  choices = variaveis_disponiveis_UI, selected = dependente_agua),
      hr(),
      h4("Correlação"),
      selectInput("var_corr_vars", "Variáveis para Matriz:",
                  choices = variaveis_disponiveis_UI, 
                  multiple = TRUE,
                  selected = c(dependente_esgoto, dependente_agua, "receita_operacional_direta_esgoto", "receita_operacional_direta_agua"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart (por UF)", plotOutput("bar_chart")),
        tabPanel("Missing Values", plotOutput("missing_values")),
        tabPanel("Descriptive Statistics", htmlOutput("descriptive_stats")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("VIF Plot", plotOutput("vif_plot")), 
        tabPanel("Extreme Observations", htmlOutput("extreme_obs")), 
        
        tabPanel("Seleção de Variáveis (Stepwise)", 
                 sidebarLayout(
                   sidebarPanel(
                     width = 4,
                     selectInput("step_dep", "Variável Dependente:", 
                                 choices = c("População Atendida Esgoto" = dependente_esgoto, 
                                             "População Atendida Água" = dependente_agua)),
                     selectInput("step_direction", "Direção:", 
                                 choices = c("both", "backward", "forward")),
                     actionButton("run_step", "Rodar Stepwise", class = "btn-primary")
                   ),
                   mainPanel(
                     width = 8,
                     verbatimTextOutput("stepwise_result"),
                     h5("Nota: O Stepwise utiliza OLS (Pooling).")
                   )
                 )
        ),
        
        tabPanel("By Group Bar Graph", plotOutput("by_group_bar")),
        tabPanel("Trend Graph", plotOutput("trend_graph")),
        tabPanel("Quantile Trend Graph", plotOutput("quantile_trend")),
        tabPanel("By Group Trend Graph", plotOutput("by_group_trend")),
        tabPanel("Correlation Graph", plotlyOutput("correlation_plot")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot")),
        
        tabPanel("Regression Table", 
                 htmlOutput("regression_table"), 
                 htmlOutput("model_criteria"), # AIC/BIC
                 htmlOutput("regression_tests")),
        
        tabPanel("Dados Completos", DT::dataTableOutput("full_data"))
      )
    )
  )
)

# ==============================================================================
# Server
server <- function(input, output, session) {
  
  # Carregar e preparar dados
  sample_data <- reactive({
    tce_raw <- read.csv(GITHUB_CSV_URL, stringsAsFactors = FALSE) %>%
      janitor::clean_names() 
    
    # Renomeação Defensiva
    tce_clean <- tce_raw %>%
      rename(
        Ano_Ref = ano,                   
        Municipio = id_municipio,
        Sigla_UF = sigla_uf
      ) %>%
      rename(
        !!RECEITA_OP_TOTAL_SAFE := receita_operacional,
        !!RECEITA_OP_INDIR_SAFE := receita_operacional_indireta,
        !!ARRECADACAO_SAFE := arrecadacao_total,
        !!CREDITO_RECEBER_SAFE := credito_areceber,
        !!DESPESA_EXPLORACAO_SAFE := despesa_exploracao,
        !!DESPESA_TOTAL_SERVICO_SAFE := despesa_total_servico,
        !!DESPESA_FISCAL_SAFE := despesa_fiscal
      ) %>%
      dplyr::select(any_of(variaveis_a_manter_no_df))
    
    # CONVERSÃO FORÇADA DE NUMÉRICOS
    cols_to_numeric <- setdiff(names(tce_clean), c("Ano_Ref", "Municipio", "Sigla_UF"))
    
    tce_clean[cols_to_numeric] <- lapply(tce_clean[cols_to_numeric], function(x) {
      as.numeric(as.character(x))
    })
    
    # Criação do df_def para ExPanDaR
    df_def_base <- data.frame(var_name = names(tce_clean))
    df_def_base$type <- sapply(names(tce_clean), function(var_name) {
      col <- tce_clean[[var_name]]
      if (var_name == "Ano_Ref") return("ts_id")
      if (var_name == "Municipio") return("cs_id")
      if (var_name == "Sigla_UF") return("factor")
      return("numeric") 
    })
    
    df_def_base$can_be_na <- !(df_def_base$var_name %in% c("Ano_Ref", "Municipio"))
    df_def_base$var_def <- df_def_base$var_name
    
    list(df = tce_clean, df_def = df_def_base)
  })
  
  # Variável reativa para a Regressão
  regression_data <- reactive({
    df <- sample_data()$df
    idvs_selected <- input$var_reg_idvs
    
    map_to_safe_name <- function(v) {
      if (v == "receita_operacional") return(RECEITA_OP_TOTAL_SAFE) 
      if (v == "receita_operacional_indireta") return(RECEITA_OP_INDIR_SAFE)
      if (v == "arrecadacao_total") return(ARRECADACAO_SAFE)
      if (v == "credito_areceber") return(CREDITO_RECEBER_SAFE)
      if (v == "despesa_exploracao") return(DESPESA_EXPLORACAO_SAFE)
      if (v == "despesa_total_servico") return(DESPESA_TOTAL_SERVICO_SAFE)
      if (v == "despesa_fiscal") return(DESPESA_FISCAL_SAFE)
      return(v)
    }
    
    idvs_safe <- sapply(idvs_selected, map_to_safe_name)
    reg_vars <- c("Municipio", "Ano_Ref", dependente_esgoto, dependente_agua, idvs_safe)
    df_sub <- df[, intersect(reg_vars, names(df)), drop = FALSE]
    df_sub <- as.data.frame(df_sub)
    
    if (nrow(df_sub) == 0 || !all(c(dependente_esgoto, dependente_agua) %in% names(df_sub))) {
      return(NULL)
    }
    
    pdata <- tryCatch({
      pdata.frame(df_sub, index = c("Municipio", "Ano_Ref"))
    }, error = function(e) {
      return(NULL)
    })
    return(pdata)
  })
  
  # --- Outputs ---
  
  output$bar_chart <- renderPlot({
    df <- sample_data()$df
    if ("Sigla_UF" %in% names(df)) {
      ggplot(df, aes(x = Sigla_UF)) + geom_bar(fill = "steelblue") + theme_minimal() + 
        labs(title = "Distribuição por UF")
    }
  })
  
  output$missing_values <- renderPlot({
    df <- sample_data()$df
    vars_to_plot_na <- setdiff(names(df), c("Municipio", "Sigla_UF"))
    df_na <- df %>% dplyr::select(any_of(vars_to_plot_na))
    
    if ("Ano_Ref" %in% names(df_na)) {
      df_na_long <- df_na %>%
        group_by(Ano_Ref) %>%
        summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
        pivot_longer(-Ano_Ref, names_to = "Variavel", values_to = "Porcentagem_NA")
      
      ggplot(df_na_long, aes(x = as.factor(Ano_Ref), y = Variavel, fill = Porcentagem_NA)) +
        geom_tile() + scale_fill_gradient(low = "lightgreen", high = "red", limits = c(0, 100)) +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(size = 8)) +
        labs(title = "Heatmap de Valores Ausentes (Missing Values)", x = "Ano", y = "Variável")
    }
  })
  
  output$descriptive_stats <- renderUI({
    HTML(prepare_descriptive_table(sample_data()$df)$kable_ret)
  })
  
  output$histogram <- renderPlot({
    df <- sample_data()$df
    if (input$var_hist %in% names(df)) {
      hist(as.numeric(df[[input$var_hist]]), col = "red", main = paste("Histograma:", input$var_hist), xlab = input$var_hist)
    }
  })
  
  output$vif_plot <- renderPlot({
    df_reg <- regression_data()
    idvs_selected <- input$var_reg_idvs
    if (is.null(df_reg) || length(idvs_selected) < 2) return(NULL)
    
    map_to_safe_name <- function(v) {
      if (v == "receita_operacional") return(RECEITA_OP_TOTAL_SAFE) 
      if (v == "receita_operacional_indireta") return(RECEITA_OP_INDIR_SAFE)
      if (v == "arrecadacao_total") return(ARRECADACAO_SAFE)
      if (v == "credito_areceber") return(CREDITO_RECEBER_SAFE)
      if (v == "despesa_exploracao") return(DESPESA_EXPLORACAO_SAFE)
      if (v == "despesa_total_servico") return(DESPESA_TOTAL_SERVICO_SAFE)
      if (v == "despesa_fiscal") return(DESPESA_FISCAL_SAFE)
      return(v)
    }
    idvs_safe <- sapply(idvs_selected, map_to_safe_name)
    formula_reg <- as.formula(paste(dependente_esgoto, "~", paste(idvs_safe, collapse = " + ")))
    
    model_ols <- tryCatch(lm(formula_reg, data = na.omit(as.data.frame(df_reg))), error = function(e) NULL)
    
    if (!is.null(model_ols)) {
      vif_val <- vif(model_ols)
      if (length(vif_val) > 0) {
        vif_df <- data.frame(Variable = names(vif_val), VIF = vif_val)
        ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF, fill = VIF)) +
          geom_bar(stat = "identity") + coord_flip() + theme_minimal() +
          labs(title = "VIF (Multicolinearidade)", x = "Variável")
      }
    }
  })
  
  # --- CORREÇÃO EXTREME OBS ---
  output$extreme_obs <- renderUI({
    df <- sample_data()$df
    var_name <- input$var_hist 
    
    if (is.null(var_name) || !(var_name %in% names(df))) return(HTML("<p>Variável não encontrada.</p>"))
    
    if (!is.numeric(df[[var_name]])) return(HTML(paste("<p>A variável", var_name, "não é numérica.</p>")))
    
    vars <- c("Municipio", "Ano_Ref", var_name)
    df_sub <- df[, vars, drop = FALSE]
    df_sub <- df_sub[complete.cases(df_sub), , drop = FALSE]
    
    if (nrow(df_sub) == 0) return(HTML("<p>Dados insuficientes (apenas NAs) para esta variável.</p>"))
    
    tab <- prepare_ext_obs_table(df_sub, var = var_name)
    HTML(tab$kable_ret)
  })
  
  # --- STEPWISE ---
  output$stepwise_result <- renderPrint({
    input$run_step 
    isolate({
      df <- sample_data()$df
      df_step <- df %>% dplyr::select(where(is.numeric)) %>% dplyr::select(-any_of(c("Ano_Ref")))
      df_step <- na.omit(df_step)
      
      if (nrow(df_step) < 20) {
        cat("Dados insuficientes para Stepwise (muitos NAs nas variáveis selecionadas).")
        return(NULL)
      }
      
      target_var <- input$step_dep
      if (!(target_var %in% names(df_step))) {
        cat(paste("Variável dependente", target_var, "não disponível para Stepwise."))
        return(NULL)
      }
      
      formula_full <- as.formula(paste(target_var, "~ ."))
      full_model <- lm(formula_full, data = df_step)
      null_model <- lm(as.formula(paste(target_var, "~ 1")), data = df_step)
      
      step_res <- stepAIC(null_model, 
                          scope = list(lower = null_model, upper = full_model), 
                          direction = input$step_direction, 
                          trace = 0) 
      
      print(summary(step_res))
      cat("\n--- Fórmula Sugerida (AIC Otimizado) ---\n")
      print(formula(step_res))
    })
  })
  
  output$by_group_bar <- renderPlot({
    df <- sample_data()$df
    if ("Sigla_UF" %in% names(df)) {
      prepare_by_group_bar_graph(df, "Sigla_UF", input$var_group, median, TRUE)$plot +
        ggtitle("Mediana por UF")
    }
  })
  
  output$trend_graph <- renderPlot({
    df <- sample_data()$df
    vars <- intersect(input$var_trend, names(df))
    if (length(vars) > 0) prepare_trend_graph(df, "Ano_Ref", vars)$plot
  })
  
  output$quantile_trend <- renderPlot({
    df <- sample_data()$df
    if (input$var_quantile %in% names(df)) {
      prepare_quantile_trend_graph(df, "Ano_Ref", c(0.05, 0.25, 0.5, 0.75, 0.95), input$var_quantile)$plot
    }
  })
  
  output$by_group_trend <- renderPlot({
    df <- sample_data()$df
    if (all(c("Sigla_UF", dependente_esgoto) %in% names(df))) {
      prepare_by_group_trend_graph(df, "Ano_Ref", "Sigla_UF", dependente_esgoto)$plot
    }
  })
  
  # --- CORREÇÃO DO GRÁFICO DE CORRELAÇÃO ---
  output$correlation_plot <- renderPlotly({
    
    df <- sample_data()$df
    vars_sel <- intersect(input$var_corr_vars, names(df))
    
    if (length(vars_sel) < 2) return(NULL)
    
    df_cor <- df %>% dplyr::select(any_of(vars_sel))
    
    # 1. Força numérico
    df_cor[] <- lapply(df_cor, function(x) as.numeric(as.character(x)))
    
    # 2. Remove NAs
    df_cor <- na.omit(df_cor)
    
    # 3. SEGURANÇA: Remove colunas constantes (Desvio Padrão = 0)
    df_cor <- df_cor %>% dplyr::select(where(~ sd(., na.rm = TRUE) > 0))
    
    # Verifica se sobraram dados
    if (ncol(df_cor) < 2 || nrow(df_cor) < 2) return(NULL)
    
    # Cálculos
    cor_mat <- cor(df_cor)
    p_mat <- cor_pvalue_matrix(df_cor)
    
    # Formato Longo
    cor_df <- as.data.frame(cor_mat) %>% 
      tibble::rownames_to_column("Var1") %>% 
      pivot_longer(-Var1, names_to = "Var2", values_to = "Cor")
    
    # --- AQUI ESTAVA O ERRO: names_to deve ser "Var2", não "Pval" ---
    p_df <- as.data.frame(p_mat) %>% 
      tibble::rownames_to_column("Var1") %>% 
      pivot_longer(-Var1, names_to = "Var2", values_to = "Pval")
    
    plot_data <- left_join(cor_df, p_df, by = c("Var1", "Var2")) %>%
      mutate(Txt = paste0("R: ", round(Cor, 3), "<br>P: ", scales::scientific(Pval)))
    
    gg <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = Cor, text = Txt)) +
      geom_tile(color = "white") + 
      scale_fill_gradient2(limit = c(-1, 1), low="red", mid="white", high="blue") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_fixed()
    
    ggplotly(gg, tooltip = "text")
  })
  
  output$scatter_plot <- renderPlot({
    df <- sample_data()$df
    req_vars <- c("Municipio", "Ano_Ref", "Sigla_UF", input$var_scatter_x, input$var_scatter_y)
    if (all(req_vars %in% names(df))) {
      df_sub <- na.omit(df[, req_vars])
      prepare_scatter_plot(df_sub, input$var_scatter_x, input$var_scatter_y, color = "Sigla_UF", size = input$var_scatter_x, loess = 1)
    }
  })
  
  output$regression_table <- renderUI({
    pdata <- regression_data()
    if (is.null(pdata)) return(HTML("<p>Dados insuficientes para regressão. Verifique a lista de variáveis.</p>"))
    
    idvs_selected <- input$var_reg_idvs
    map_to_safe_name <- function(v) {
      if (v == "receita_operacional") return(RECEITA_OP_TOTAL_SAFE) 
      if (v == "receita_operacional_indireta") return(RECEITA_OP_INDIR_SAFE)
      if (v == "arrecadacao_total") return(ARRECADACAO_SAFE)
      if (v == "credito_areceber") return(CREDITO_RECEBER_SAFE)
      if (v == "despesa_exploracao") return(DESPESA_EXPLORACAO_SAFE)
      if (v == "despesa_total_servico") return(DESPESA_TOTAL_SERVICO_SAFE)
      if (v == "despesa_fiscal") return(DESPESA_FISCAL_SAFE)
      return(v)
    }
    idvs_safe <- sapply(idvs_selected, map_to_safe_name)
    idvs <- intersect(idvs_safe, names(pdata))
    
    if (length(idvs) == 0) return(NULL)
    
    f_esgoto <- as.formula(paste(dependente_esgoto, "~", paste(idvs, collapse = "+")))
    f_agua <- as.formula(paste(dependente_agua, "~", paste(idvs, collapse = "+")))
    
    run_model <- function(formula) {
      switch(input$model_type,
             "OLS" = plm(formula, pdata, model="pooling"),
             "Efeitos Fixos (FE)" = plm(formula, pdata, model="within"),
             "Efeitos Aleatórios (RE)" = plm(formula, pdata, model="random"))
    }
    
    model_esgoto <- tryCatch(run_model(f_esgoto), error=function(e) NULL)
    model_agua   <- tryCatch(run_model(f_agua), error=function(e) NULL)
    
    lista_modelos <- list()
    if(!is.null(model_esgoto)) lista_modelos[["Esgoto"]] <- model_esgoto
    if(!is.null(model_agua))   lista_modelos[["Agua"]] <- model_agua
    
    if (length(lista_modelos) == 0) return(HTML("<p>Erro ao calcular modelos. Verifique singularidade ou NAs.</p>"))
    
    HTML(paste(capture.output(stargazer(lista_modelos, type="html", 
                                        title="Comparativo de Regressão (Esgoto vs Água)",
                                        column.labels = c("Pop. Esgoto", "Pop. Água"))), collapse="\n"))
  })
  
  output$model_criteria <- renderUI({
    pdata <- regression_data()
    if (is.null(pdata) || input$model_type != "OLS") return(NULL) 
    HTML("<h4>Critérios de Informação (Apenas OLS)</h4><p>AIC e BIC são calculados apenas para modelos Pooled OLS.</p>")
  })
  
  output$regression_tests <- renderUI({
    pdata <- regression_data()
    if (is.null(pdata) || input$model_type == "OLS") return(NULL)
    
    idvs_selected <- input$var_reg_idvs
    map_to_safe_name <- function(v) {
      if (v == "receita_operacional") return(RECEITA_OP_TOTAL_SAFE) 
      if (v == "receita_operacional_indireta") return(RECEITA_OP_INDIR_SAFE)
      if (v == "arrecadacao_total") return(ARRECADACAO_SAFE)
      if (v == "credito_areceber") return(CREDITO_RECEBER_SAFE)
      if (v == "despesa_exploracao") return(DESPESA_EXPLORACAO_SAFE)
      if (v == "despesa_total_servico") return(DESPESA_TOTAL_SERVICO_SAFE)
      if (v == "despesa_fiscal") return(DESPESA_FISCAL_SAFE)
      return(v)
    }
    idvs <- intersect(sapply(idvs_selected, map_to_safe_name), names(pdata))
    
    generate_tests <- function(dv_name, label) {
      f <- as.formula(paste(dv_name, "~", paste(idvs, collapse = "+")))
      fe <- tryCatch(plm(f, pdata, model="within"), error=function(e) NULL)
      re <- tryCatch(plm(f, pdata, model="random"), error=function(e) NULL)
      ols <- tryCatch(plm(f, pdata, model="pooling"), error=function(e) NULL)
      
      res <- paste0("<h4>Diagnósticos para ", label, "</h4>")
      if(!is.null(fe) && !is.null(ols)) {
        ft <- pFtest(fe, ols)
        res <- paste0(res, "<p><b>Teste F (FE vs OLS):</b> P-valor: ", round(ft$p.value, 4), 
                      ifelse(ft$p.value < 0.05, " (Prefere FE)", " (Prefere OLS)"), "</p>")
      }
      if(!is.null(fe) && !is.null(re)) {
        ht <- phtest(fe, re)
        res <- paste0(res, "<p><b>Teste de Hausman (FE vs RE):</b> P-valor: ", round(ht$p.value, 4), 
                      ifelse(ht$p.value < 0.05, " (Prefere FE)", " (Prefere RE)"), "</p>")
      }
      return(res)
    }
    
    res_esgoto <- generate_tests(dependente_esgoto, "Esgoto")
    res_agua   <- generate_tests(dependente_agua, "Água")
    
    HTML(paste(res_esgoto, "<hr>", res_agua))
  })
  
  output$full_data <- DT::renderDataTable({ sample_data()$df }, options = list(scrollX = TRUE))
}

shinyApp(ui, server)

