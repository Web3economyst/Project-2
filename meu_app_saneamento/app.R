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
library(bslib)
library(rlang)
library(sandwich)
library(glmnet)

# ==============================================================================
# TEMA E CSS
# ==============================================================================
my_theme <- bs_theme(
  version = 5,
  bootswatch = "zephyr",
  primary = "#2c3e50",
  secondary = "#18bc9c",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat")
) %>%
  bs_add_rules(
    ".selectize-control.multi .selectize-input > div { 
      display: block !important; 
      margin: 2px 0 !important;
      white-space: normal !important;
      word-break: break-all !important;
    }
    .selectize-input { max-height: 250px; overflow-y: auto !important; }"
  )

# ==============================================================================
# FUNÇÕES AUXILIARES
# ==============================================================================
fmt_pval <- function(x) {
  if (is.null(x) || is.na(x) || is.nan(x)) return("-")
  if (x < 2.2e-16) return("< 2.2e-16 ***")
  if (x < 0.001) return(paste0(format(x, scientific = TRUE, digits = 4), " ***"))
  if (x < 0.01) return(paste0(round(x, 4), " **"))
  if (x < 0.05) return(paste0(round(x, 4), " *"))
  return(round(x, 4))
}

cor_pvalue_matrix <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      sd_i <- sd(mat[, i], na.rm = TRUE)
      sd_j <- sd(mat[, j], na.rm = TRUE)
      if (is.na(sd_i) || is.na(sd_j) || sd_i == 0 || sd_j == 0) {
        p.mat[i, j] <- p.mat[j, i] <- 1 
      } else {
        tryCatch({
          tmp <- cor.test(mat[, i], mat[, j], use = "pairwise.complete.obs")
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }, error = function(e) { p.mat[i, j] <- p.mat[j, i] <- 1 })
      }
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

# Cálculo CORRETO de AIC para Painel
get_plm_info <- function(plm_model) {
  rss <- sum(residuals(plm_model)^2)
  n <- length(residuals(plm_model))
  k <- length(coef(plm_model))
  
  args <- attr(plm_model, "args")
  if (!is.null(args$model) && args$model == "within") {
    n_entities <- length(unique(index(plm_model)[[1]]))
    k_eff <- k + n_entities
  } else {
    k_eff <- k
  }
  
  if (rss <= 0) return(c(AIC = NA, BIC = NA))
  
  aic_val <- n * log(rss / n) + 2 * k_eff
  bic_val <- n * log(rss / n) + log(n) * k_eff
  return(c(AIC = aic_val, BIC = bic_val))
}

clean_pt_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.na(x) || x == "" || x == "-") return(NA)
  x_clean <- gsub("\\.", "", x)
  x_clean <- gsub(",", ".", x_clean)
  return(suppressWarnings(as.numeric(x_clean)))
}

GITHUB_CSV_URL <- "https://github.com/Web3economyst/Project-2/raw/refs/heads/main/database_saneamento.csv"

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  theme = my_theme,
  title = "Dashboard Saneamento RS (Full Data)",
  
  sidebar = sidebar(
    title = "Filtros Globais",
    class = "bg-light",
    selectInput("global_natureza_juridica", "Natureza Jurídica:", choices = NULL, multiple = TRUE, selectize = TRUE),
    selectInput("global_estado", "Estado (UF):", choices = NULL, multiple = TRUE, selectize = TRUE),
    selectInput("global_municipio", "Município:", choices = NULL, multiple = TRUE, selectize = TRUE),
    uiOutput("ui_year_slider"),
    helpText("Use o slider para restringir o período."),
    hr(),
    div(class = "alert alert-warning", style = "font-size: 0.8em;",
        "Nota: Dados agregados por Município/Ano para viabilizar Painel.")
  ),
  
  tabsetPanel(
    type = "pills",
    
    # --- ABA 1: DADOS ---
    tabPanel("Dados & Engenharia", icon = icon("database"),
             br(),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Criar Nova Variável"),
                 textInput("new_var_name", "Nome:", placeholder = "ex: margem_ebitda"),
                 textAreaInput("new_var_formula", "Fórmula:", placeholder = "ex: (fn005_... - fn015_...) / fn005_..."),
                 actionButton("btn_create_var", "Criar Variável", class = "btn-success", width = "100%"),
                 hr(),
                 h5("Variáveis Disponíveis:"),
                 verbatimTextOutput("list_vars_available")
               ),
               mainPanel(
                 h4("Base Completa (Nomes Originais Limpos)"),
                 p(strong("Nota:"), " Variáveis 'Lucro_op' e 'desp_ppe' calculadas automaticamente."),
                 DT::dataTableOutput("full_data")
               )
             )
    ),
    
    # --- ABA 2: Visão Geral ---
    tabPanel("Visão Geral", icon = icon("chart-pie"),
             br(),
             tabsetPanel(
               tabPanel("Estatísticas", htmlOutput("descriptive_stats")),
               tabPanel("Dados Faltantes", 
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            h5("Configuração do Heatmap"),
                            selectizeInput("miss_var_select", "Selecione as Variáveis:", choices = NULL, multiple = TRUE, options = list(placeholder = "Padrão: Seleção Personalizada"))
                          ),
                          mainPanel(plotOutput("missing_values", height = "600px"))
                        )
               ),
               tabPanel("Distribuição Geográfica", plotOutput("bar_chart"))
             )
    ),
    
    # --- ABA 3: Gráficos ---
    tabPanel("Gráficos", icon = icon("chart-bar"),
             br(),
             tabsetPanel(
               tabPanel("Histograma",
                        sidebarLayout(
                          sidebarPanel(width = 3, selectInput("var_hist", "Variável:", choices = NULL)),
                          mainPanel(plotOutput("histogram"))
                        )
               ),
               tabPanel("Tendências",
                        sidebarLayout(
                          sidebarPanel(width = 3, selectInput("var_trend", "Variável Y:", choices = NULL)),
                          mainPanel(plotOutput("trend_graph"))
                        )
               ),
               tabPanel("Correlações",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3, 
                            h5("Seleção de Variáveis"),
                            selectInput("var_corr_vars", "Vars Matriz / Barras:", choices = NULL, multiple = TRUE), 
                            hr(),
                            h5("Variável Base (Foco)"),
                            selectInput("corr_focus_var", "Variável Foco (Base):", choices = NULL) 
                          ),
                          mainPanel(
                            h4("1. Matriz de Correlação (Heatmap)"),
                            plotlyOutput("correlation_plot", height = "500px"),
                            br(), hr(),
                            h4("2. Ranking de Correlação (Foco Específico)"),
                            plotlyOutput("correlation_bar_plot", height = "500px") 
                          )
                        )
               )
             )
    ),
    
    # --- ABA 4: MODELAGEM ---
    tabPanel("Modelagem & Seleção", icon = icon("cogs"),
             br(),
             tabsetPanel(
               
               # SEÇÃO 1: TESTES DE PAINEL
               tabPanel("Testes de Painel (F & Hausman)", 
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            h4("Configuração do Teste"),
                            selectInput("spec_dep", "Dependente (Y):", choices = NULL),
                            selectInput("spec_idvs", "Independentes (X):", choices = NULL, multiple = TRUE),
                            hr(),
                            actionButton("btn_run_spec", "Executar Testes", class = "btn-primary", width = "100%")
                          ),
                          mainPanel(
                            h3("Resultados de Especificação"),
                            p("Estes testes ajudam a decidir entre OLS, Efeitos Fixos ou Aleatórios."),
                            uiOutput("spec_tests_results_ui")
                          )
                        )
               ),
               
               # SEÇÃO 2: SELEÇÃO DO MODELO (STEPWISE COM ERRO ROBUSTO)
               tabPanel("Seleção do Modelo", 
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            h4("Configuração da Seleção"),
                            selectInput("step_model_type", "Modelo de Referência:", 
                                        choices = c("Fixed Effects (Within)" = "within", "Random Effects" = "random", "Pooling (OLS)" = "pooling")),
                            selectInput("step_dep", "Dependente (Y):", choices = NULL),
                            selectInput("step_idvs", "Candidatas (X):", choices = NULL, multiple = TRUE),
                            hr(),
                            div(class = "alert alert-warning",
                                p(strong("Atenção:"), "A seleção usa AIC. Para significância real, observe o quadro 'Ajuste Robusto'."),
                                tags$ul(
                                  tags$li("Transformação within aplicada"),
                                  tags$li("Critério: Seleção por AIC"),
                                  tags$li("Diagnóstico Pós-Seleção com Erros Clusterizados")
                                )
                            ),
                            actionButton("btn_run_stepwise_adv", "Executar Stepwise", class = "btn-success", width = "100%")
                          ),
                          mainPanel(
                            h3("Comparação de Métodos de Seleção"),
                            div(class="alert alert-info", "O Stepwise seleciona variáveis pelo menor AIC. Abaixo, validamos essa seleção com Erros Padrão Clusterizados para corrigir a inflação de significância."),
                            DT::dataTableOutput("stepwise_results_table"),
                            br(),
                            
                            h4("Diagnóstico de Robustez (Pós-Seleção)"),
                            p("Teste conjunto e individual usando erros robustos (HAC) para evitar falsos positivos."),
                            verbatimTextOutput("robustness_diag"),
                            
                            h4("Resumo do Modelo Vencedor (Detalhado)"),
                            verbatimTextOutput("stepwise_best_summary"),
                            
                            hr(),
                            h4("Diagnóstico de Resíduos"),
                            fluidRow(
                              column(6, plotOutput("step_resid_plot", height = "300px")),
                              column(6, plotOutput("step_qq_plot", height = "300px"))
                            ),
                            verbatimTextOutput("step_resid_tests")
                          )
                        )
               ),
               
               # SEÇÃO 3: LASSO
               tabPanel("Métodos Avançados (LASSO/Net)",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            h4("Configuração de Seleção"),
                            selectInput("lasso_dep", "Dependente (Y):", choices = NULL),
                            selectInput("lasso_idvs", "Candidatas (X):", choices = NULL, multiple = TRUE),
                            hr(),
                            selectInput("sel_method", "Método de Seleção:", 
                                        choices = c("LASSO", "Elastic Net", "Stability Selection")),
                            conditionalPanel(
                              condition = "input.sel_method == 'Elastic Net'",
                              sliderInput("el_alpha", "Alpha:", min = 0.0, max = 1.0, value = 0.5, step = 0.1)
                            ),
                            conditionalPanel(
                              condition = "input.sel_method == 'Stability Selection'",
                              sliderInput("stab_thresh", "Threshold:", min = 0.5, max = 1.0, value = 0.6, step = 0.05),
                              numericInput("stab_iter", "Iterações:", value = 50, min = 10, max = 200)
                            ),
                            hr(),
                            checkboxInput("lasso_fe", "Aplicar 'Within Transformation'?", value = FALSE),
                            helpText("Subtrai a média por município antes de rodar (Simula Efeitos Fixos)."),
                            br(),
                            actionButton("btn_run_lasso", "Executar Seleção", class = "btn-success", width = "100%")
                          ),
                          mainPanel(
                            h3("Resultado da Seleção"),
                            div(class="alert alert-info", textOutput("method_desc")),
                            fluidRow(
                              column(7, plotOutput("lasso_plot")),
                              column(5, h4("Variáveis"), tableOutput("lasso_coefs_table"))
                            )
                          )
                        )
               ),
               
               # SEÇÃO 4: REGRESSÃO MANUAL
               tabPanel("Regressão Manual (Comparativo)",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            h5("Configuração"),
                            selectInput("model_type_manual", "Tipo de Modelo:", c("Pooling (OLS)", "Fixed Effects (Within)", "Random Effects")),
                            hr(),
                            selectInput("manual_cat_fe", "1º Efeito Fixo (Index):", choices = NULL, selected = "Municipio"),
                            selectInput("manual_se_type", "Correção de Erros:", 
                                        choices = c("Padrão (Sem correção)" = "Standard", 
                                                    "Clusterizado (HAC)" = "Clustered", 
                                                    "Driscoll-Kraay (SCC)" = "Driscoll-Kraay")),
                            helpText("Clusterizado: Corrige Autocorr. e Heteroced."),
                            helpText("Driscoll-Kraay: Corrige Dep. Espacial e Temporal."),
                            hr(),
                            selectInput("manual_dep", "Dependente (Y):", choices = NULL),
                            selectInput("manual_idvs", "Independentes (X):", choices = NULL, multiple = TRUE),
                            hr(),
                            div(class="alert alert-info", "Gera colunas para cada Natureza Jurídica.")
                          ),
                          mainPanel(
                            h4("Resultados: Geral vs. Por Natureza Jurídica"),
                            htmlOutput("regression_table_manual"),
                            htmlOutput("manual_diagnostics")
                          )
                        )
               ),
               
               # SEÇÃO 5: DIAGNÓSTICOS PÓS-FE
               tabPanel("Diagnósticos (Pós-FE)",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            selectInput("diag_cat_fe", "Efeito Fixo (ID):", choices = NULL, selected = "Municipio"),
                            selectInput("diag_dep", "Dependente (Y):", choices = NULL),
                            selectInput("diag_idvs", "Independentes (X):", choices = NULL, multiple = TRUE),
                            actionButton("btn_run_diag", "Rodar Diagnósticos", class = "btn-warning", width = "100%")
                          ),
                          mainPanel(
                            h3("Diagnósticos Essenciais Após o FE"),
                            p("Avaliação dos resíduos para validar as hipóteses do modelo."),
                            hr(),
                            uiOutput("diag_results_ui")
                          )
                        )
               ),
               
               # SEÇÃO 6: VIF
               tabPanel("Teste VIF",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            selectInput("vif_idvs", "Variáveis Independentes (X):", choices = NULL, multiple = TRUE),
                            actionButton("btn_calc_vif", "Calcular VIF", class = "btn-info", width = "100%")
                          ),
                          mainPanel(
                            h3("Fatores de Inflação da Variância (VIF)"),
                            verbatimTextOutput("vif_diagnosis"),
                            fluidRow(
                              column(6, tableOutput("vif_result_table")),
                              column(6, plotOutput("vif_plot"))
                            )
                          )
                        )
               )
             )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  values <- reactiveValues(raw_df = NULL, col_names = NULL)
  step_results_data <- reactiveVal(NULL)
  best_step_model <- reactiveVal(NULL)
  best_method_name <- reactiveVal(NULL)
  
  # --- 1. CARREGAMENTO E PROCESSAMENTO ---
  observe({
    req(is.null(values$raw_df))
    id <- showNotification("Carregando base...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    raw <- tryCatch({ read.csv(GITHUB_CSV_URL, stringsAsFactors = FALSE, fileEncoding = "latin1", check.names = FALSE) }, 
                    error = function(e) { read.csv(GITHUB_CSV_URL, stringsAsFactors = FALSE, check.names = FALSE) })
    
    clean <- raw %>% janitor::clean_names()
    if("municipio" %in% names(clean)) names(clean)[names(clean) == "municipio"] <- "Municipio"
    if("estado" %in% names(clean)) names(clean)[names(clean) == "estado"] <- "Sigla_UF"
    if("ano_de_referencia" %in% names(clean)) names(clean)[names(clean) == "ano_de_referencia"] <- "Ano_Ref"
    if("natureza_juridica" %in% names(clean)) names(clean)[names(clean) == "natureza_juridica"] <- "Natureza_Juridica"
    
    cols_skip <- c("Municipio", "Sigla_UF", "Natureza_Juridica", "prestador", "sigla_do_prestador")
    for(col in setdiff(names(clean), cols_skip)) if(is.character(clean[[col]])) clean[[col]] <- sapply(clean[[col]], clean_pt_num)
    
    c2 <- names(clean)[grepl("^fn002", names(clean))][1]
    c15 <- names(clean)[grepl("^fn015", names(clean))][1]
    if(!is.na(c2) && !is.na(c15)) clean$Lucro_op <- clean[[c2]] - clean[[c15]]
    
    c11 <- names(clean)[grepl("^fn011", names(clean))][1]; c10 <- names(clean)[grepl("^fn010", names(clean))][1]; c13 <- names(clean)[grepl("^fn013", names(clean))][1]
    if(!is.na(c11) && !is.na(c10) && !is.na(c13)) clean$desp_ppe <- clean[[c11]] + clean[[c10]] + clean[[c13]]
    
    values$raw_df <- clean
    values$col_names <- names(clean)
    
    updateSelectInput(session, "global_natureza_juridica", choices = sort(unique(clean$Natureza_Juridica)))
    updateSelectInput(session, "global_estado", choices = sort(unique(clean$Sigla_UF)))
    
    char_cols <- names(clean)[sapply(clean, function(x) is.character(x) || is.factor(x))]
    sel_def <- "Municipio"
    updateSelectInput(session, "manual_cat_fe", choices = char_cols, selected = sel_def)
    updateSelectInput(session, "diag_cat_fe", choices = char_cols, selected = sel_def)
  })
  
  # --- 2. DATA FILTERED ---
  data_filtered <- reactive({
    req(values$raw_df)
    df <- values$raw_df
    if (!is.null(input$global_natureza_juridica)) df <- df %>% filter(Natureza_Juridica %in% input$global_natureza_juridica)
    if (!is.null(input$global_estado)) df <- df %>% filter(Sigla_UF %in% input$global_estado)
    if (!is.null(input$global_municipio)) df <- df %>% filter(Municipio %in% input$global_municipio)
    if (!is.null(input$global_ano_range)) df <- df %>% filter(Ano_Ref >= input$global_ano_range[1] & Ano_Ref <= input$global_ano_range[2])
    
    df %>%
      group_by(Municipio, Ano_Ref) %>%
      summarise(across(where(is.numeric), ~sum(., na.rm = TRUE)), 
                Natureza_Juridica = first(Natureza_Juridica), 
                Sigla_UF = first(Sigla_UF), .groups = "drop")
  })
  
  # --- 3. DEFAULTS ---
  observeEvent(values$col_names, {
    req(values$raw_df)
    nums <- names(values$raw_df)[sapply(values$raw_df, is.numeric)]
    
    target_y <- nums[grepl("es001", nums, ignore.case = TRUE)][1]
    if(is.na(target_y)) target_y <- "Lucro_op"
    
    target_x_codes <- c("fn003", "fn006", "fn008", "fn010", "fn011", "fn013", 
                        "fn014", "fn024", "fn043", "fn053", "es028", "fn027", 
                        "fn020", "fn039", "fn021")
    target_x <- c()
    for(code in target_x_codes) {
      found <- nums[grepl(code, nums, ignore.case = TRUE)][1]
      if(!is.na(found)) target_x <- c(target_x, found)
    }
    if(length(target_x) == 0) target_x <- nums[1:5]
    
    updateSelectizeInput(session, "miss_var_select", choices = nums, selected = target_x[1:5])
    updateSelectInput(session, "spec_dep", choices = nums, selected = target_y)
    updateSelectInput(session, "spec_idvs", choices = nums, selected = target_x[1:min(length(target_x), 5)])
    updateSelectInput(session, "step_dep", choices = nums, selected = target_y)
    updateSelectInput(session, "step_idvs", choices = nums, selected = target_x)
    updateSelectInput(session, "manual_dep", choices = nums, selected = target_y)
    updateSelectInput(session, "manual_idvs", choices = nums, selected = target_x)
    updateSelectInput(session, "lasso_dep", choices = nums, selected = target_y)
    updateSelectInput(session, "lasso_idvs", choices = nums, selected = target_x)
    updateSelectInput(session, "diag_dep", choices = nums, selected = target_y)
    updateSelectInput(session, "diag_idvs", choices = nums, selected = target_x[1:min(length(target_x), 5)])
    updateSelectInput(session, "vif_idvs", choices = nums, selected = target_x[1:min(length(target_x), 5)])
    updateSelectInput(session, "var_hist", choices = nums, selected = target_y)
    updateSelectInput(session, "var_trend", choices = nums, selected = target_y)
    updateSelectInput(session, "var_corr_vars", choices = nums, selected = target_x)
    updateSelectInput(session, "corr_focus_var", choices = nums, selected = target_y)
  })
  
  output$ui_year_slider <- renderUI({
    req(values$raw_df)
    anos <- na.omit(values$raw_df$Ano_Ref)
    sliderInput("global_ano_range", "Período:", min(anos), max(anos), value = range(anos), step = 1, sep="")
  })
  
  observeEvent(input$btn_create_var, {
    req(input$new_var_name, input$new_var_formula, values$raw_df)
    tryCatch({
      new_data <- values$raw_df %>% mutate(!!sym(input$new_var_name) := !!parse_expr(input$new_var_formula))
      values$raw_df <- new_data
      values$col_names <- names(new_data)
      showNotification("Variável criada!", type = "message")
    }, error = function(e) showNotification(paste("Erro:", e$message), type = "error"))
  })
  
  # --- OUTPUTS TAB 1, 2, 3 ---
  output$list_vars_available <- renderPrint({ req(values$col_names); print(values$col_names) })
  output$full_data <- DT::renderDataTable({ req(data_filtered()); data_filtered() }, options = list(scrollX = TRUE))
  
  output$descriptive_stats <- renderUI({
    req(data_filtered())
    df_desc <- data_filtered() %>% dplyr::select(where(is.numeric))
    if(ncol(df_desc) > 0) {
      st <- do.call(data.frame, 
                    list(Mean = apply(df_desc, 2, mean, na.rm=TRUE),
                         SD = apply(df_desc, 2, sd, na.rm=TRUE),
                         Min = apply(df_desc, 2, min, na.rm=TRUE),
                         Max = apply(df_desc, 2, max, na.rm=TRUE)))
      kable(st, format = "html", digits = 2) %>% kable_styling("striped") %>% HTML()
    }
  })
  
  output$missing_values <- renderPlot({
    req(values$raw_df, input$miss_var_select)
    df_raw_filt <- values$raw_df
    if (!is.null(input$global_natureza_juridica)) df_raw_filt <- df_raw_filt %>% filter(Natureza_Juridica %in% input$global_natureza_juridica)
    if (!is.null(input$global_estado)) df_raw_filt <- df_raw_filt %>% filter(Sigla_UF %in% input$global_estado)
    if (!is.null(input$global_municipio)) df_raw_filt <- df_raw_filt %>% filter(Municipio %in% input$global_municipio)
    if (!is.null(input$global_ano_range)) df_raw_filt <- df_raw_filt %>% filter(Ano_Ref >= input$global_ano_range[1] & Ano_Ref <= input$global_ano_range[2])
    
    df <- df_raw_filt %>% dplyr::select(any_of(input$miss_var_select), Ano_Ref)
    df_long <- df %>% group_by(Ano_Ref) %>% summarise(across(everything(), ~sum(is.na(.))/n()*100)) %>% pivot_longer(-Ano_Ref)
    
    ggplot(df_long, aes(x=factor(Ano_Ref), y=name, fill=value)) + 
      geom_tile(color = "white", linewidth = 0.5) + # Borda
      geom_text(aes(label = paste0(round(value, 0), "%")), size = 3.5, fontface = "bold",
                color = ifelse(df_long$value > 50, "white", "black")) + 
      scale_fill_gradient(low = "#ecf0f1", high = "#c0392b", limits = c(0, 100), name = "% Missing") +
      labs(title = "Mapa de Dados Faltantes (Por Ano)", x = NULL, y = NULL) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 0, face = "bold"),
            panel.grid = element_blank())
  })
  
  output$bar_chart <- renderPlot({
    req(data_filtered())
    data_filtered() %>% count(Sigla_UF) %>% ggplot(aes(x = reorder(Sigla_UF, -n), y = n)) + geom_col(fill = "#2c3e50") + theme_minimal()
  })
  
  output$histogram <- renderPlot({
    req(data_filtered(), input$var_hist)
    ggplot(data_filtered(), aes(x = .data[[input$var_hist]])) + geom_histogram(fill = "#18bc9c", bins=30) + theme_minimal()
  })
  
  output$trend_graph <- renderPlot({
    req(data_filtered(), input$var_trend)
    df <- data_filtered() %>% group_by(Ano_Ref) %>% summarise(M = mean(.data[[input$var_trend]], na.rm=T))
    ggplot(df, aes(x=Ano_Ref, y=M)) + geom_line(linewidth=1) + geom_point() + theme_minimal()
  })
  
  output$correlation_plot <- renderPlotly({
    req(data_filtered(), input$var_corr_vars)
    df_sel <- data_filtered() %>% dplyr::select(any_of(input$var_corr_vars)) %>% dplyr::select(where(is.numeric)) %>% na.omit()
    if(ncol(df_sel) < 2) return(NULL)
    cor_mat <- cor(df_sel)
    p_mat <- cor_pvalue_matrix(df_sel)
    cor_df <- as.data.frame(cor_mat) %>% tibble::rownames_to_column("Var1") %>% pivot_longer(-Var1, names_to = "Var2", values_to = "Cor")
    p_df <- as.data.frame(p_mat) %>% tibble::rownames_to_column("Var1") %>% pivot_longer(-Var1, names_to = "Var2", values_to = "Pval")
    plot_data <- left_join(cor_df, p_df, by = c("Var1", "Var2")) %>% mutate(Txt = paste0("R: ", round(Cor, 3), "<br>P: ", scales::scientific(Pval)))
    gg <- ggplot(plot_data, aes(x=Var1, y=Var2, fill=Cor, text=Txt)) + geom_tile() + scale_fill_gradient2(limit=c(-1,1)) + theme_minimal() + coord_fixed() + theme(axis.text.x = element_text(angle=45))
    ggplotly(gg, tooltip = "text")
  })
  
  output$correlation_bar_plot <- renderPlotly({
    req(data_filtered(), input$corr_focus_var)
    df_num <- data_filtered() %>% dplyr::select(where(is.numeric))
    if(!input$corr_focus_var %in% names(df_num)) return(NULL)
    vars <- setdiff(input$var_corr_vars, input$corr_focus_var)
    if(length(vars) == 0) return(NULL)
    cors <- sapply(vars, function(v) cor(df_num[[input$corr_focus_var]], df_num[[v]], use="pairwise.complete.obs"))
    plot_data <- data.frame(Var=names(cors), Cor=cors)
    gg <- ggplot(plot_data, aes(x=reorder(Var, Cor), y=Cor, fill=Cor)) + geom_col() + coord_flip() + scale_fill_gradient2(limit=c(-1,1)) + theme_minimal()
    ggplotly(gg)
  })
  
  # --- OUTPUTS TAB 4 ---
  
  # 1. Testes de Painel
  output$spec_tests_results_ui <- renderUI({
    input$btn_run_spec
    isolate({
      req(input$spec_dep, input$spec_idvs)
      df <- data_filtered() %>% dplyr::select(Municipio, Ano_Ref, all_of(input$spec_dep), all_of(input$spec_idvs)) %>% na.omit()
      df <- df %>% dplyr::select(where(~is.numeric(.) && var(.) > 0), Municipio, Ano_Ref)
      if(ncol(df) < 3) return(div(class="alert alert-danger", "Erro: Variáveis constantes selecionadas."))
      
      pdata <- pdata.frame(df, index = c("Municipio", "Ano_Ref"))
      form <- as.formula(paste(input$spec_dep, "~", paste(input$spec_idvs, collapse = "+")))
      
      tryCatch({
        mod_fe <- plm(form, data = pdata, model = "within")
        mod_re <- plm(form, data = pdata, model = "random")
        mod_pool <- plm(form, data = pdata, model = "pooling")
        
        f_t <- pFtest(mod_fe, mod_pool)
        h_t <- phtest(mod_re, mod_fe)
        
        tagList(
          h4("Teste F (Pooled vs FE)"), p(paste("P-valor:", fmt_pval(f_t$p.value))),
          p(if(f_t$p.value < 0.05) "Rejeita H0: Use Efeitos Fixos." else "Não Rejeita H0: Use Pooling."),
          hr(),
          h4("Teste Hausman (RE vs FE)"), p(paste("P-valor:", fmt_pval(h_t$p.value))),
          p(if(h_t$p.value < 0.05) "Rejeita H0 (RE Inconsistente): Use Efeitos Fixos (Within)." else "Não Rejeita H0: Use Efeitos Aleatórios (Eficiente).")
        )
      }, error = function(e) {
        if(grepl("singular", e$message)) {
          div(class="alert alert-danger", "Erro: Matriz singular. Isso ocorre quando variáveis são redundantes (colinearidade perfeita). Remova variáveis que sejam combinação linear de outras.")
        } else {
          div(class="alert alert-danger", paste("Erro:", e$message))
        }
      })
    })
  })
  
  # 2. SELEÇÃO DO MODELO (STEPWISE - COM LOGICA CORRETA)
  observeEvent(input$btn_run_stepwise_adv, {
    req(input$step_dep, input$step_idvs)
    
    df <- data_filtered() %>%
      dplyr::select(Municipio, Ano_Ref, all_of(input$step_dep), all_of(input$step_idvs)) %>%
      na.omit()
    
    if(nrow(df) < 30) {
      showNotification("Amostra muito pequena (n < 30).", type = "warning")
      return(NULL)
    }
    if(length(unique(df$Municipio)) < 5) {
      showNotification("Número insuficiente de grupos para painel.", type = "warning")
      return(NULL)
    }
    
    if(input$step_model_type == "within") {
      df_step <- df %>%
        group_by(Municipio) %>%
        mutate(across(all_of(c(input$step_dep, input$step_idvs)), ~ . - mean(., na.rm = TRUE))) %>%
        ungroup()
    } else {
      df_step <- df
    }
    
    null_model <- lm(as.formula(paste(input$step_dep, "~ 1")), data = df_step)
    full_model <- lm(as.formula(paste(input$step_dep, "~", paste(input$step_idvs, collapse = "+"))), data = df_step)
    
    directions <- c("forward", "backward", "both")
    results_list <- list()
    best_aic <- Inf
    best_mod <- NULL
    best_name <- ""
    
    pdata <- pdata.frame(df, index = c("Municipio", "Ano_Ref"))
    
    withProgress(message = "Calculando Stepwise...", {
      for (dir in directions) {
        incProgress(1/3, detail = dir)
        
        start_mod <- if (dir == "forward") null_model else full_model
        step_res <- stepAIC(start_mod, scope = list(lower = null_model, upper = full_model), direction = dir, trace = FALSE)
        
        final_vars <- names(coef(step_res))[-1]
        if (length(final_vars) == 0) next
        
        final_form <- as.formula(paste(input$step_dep, "~", paste(final_vars, collapse = "+")))
        
        mod_p <- tryCatch(
          plm(final_form, data = pdata, model = input$step_model_type),
          error = function(e) NULL
        )
        
        if (!is.null(mod_p)) {
          info <- get_plm_info(mod_p)
          
          results_list[[dir]] <- data.frame(
            Metodo = dir,
            AIC = round(info["AIC"], 2),
            BIC = round(info["BIC"], 2),
            Variaveis = paste(final_vars, collapse = " + ")
          )
          
          if (info["AIC"] < best_aic) {
            best_aic <- info["AIC"]
            best_mod <- mod_p
            best_name <- dir
          }
        }
      }
    })
    
    if (length(results_list) > 0) {
      step_results_data(do.call(rbind, results_list))
      best_step_model(best_mod)
      best_method_name(best_name)
      
      tryCatch({
        vcov_mat <- tryCatch(vcovHC(best_mod, type="HC1", cluster="group"), error=function(e) NULL)
        msg_err <- " (Matriz HAC clusterizada por grupo)"
        if(is.null(vcov_mat)) {
          vcov_mat <- vcov(best_mod)
          msg_err <- " (Matriz singular: usando erros padrão simples - Cuidado com inferência)"
        }
        
        ct <- coeftest(best_mod, vcov=vcov_mat)
        sig_vars <- rownames(ct)[ct[,4] < 0.05]
        
        vars_names <- names(coef(best_mod))
        p_joint <- NA
        if(length(vars_names) > 0) {
          try({
            lh <- car::linearHypothesis(best_mod, vars_names, vcov.=vcov_mat)
            p_joint <- lh[2, "Pr(>Chisq)"]
          }, silent=TRUE)
        }
        
        output$robustness_diag <- renderPrint({
          cat("=== DIAGNÓSTICO DE ROBUSTEZ (Pós-Seleção) ===\n")
          cat("Método de Erro Padrão:", msg_err, "\n")
          cat("Variáveis selecionadas:", length(vars_names), "\n")
          cat("Variáveis significativas (p < 0.05):", length(sig_vars), "\n")
          
          if(!is.na(p_joint)) {
            cat("Teste Conjunto (Wald) P-valor:", format.pval(p_joint, digits=4), "\n")
            if(p_joint < 0.05) cat("→ Modelo conjunto é significativo (OK)\n")
            else cat("→ ATENÇÃO: Modelo pode não ser estatisticamente significativo.\n")
          } else {
            cat("Teste conjunto não pôde ser calculado (possível singularidade).\n")
          }
        })
      }, error=function(e) output$robustness_diag <- renderPrint(paste("Erro no diagnóstico:", e$message)))
    }
  })
  
  output$stepwise_results_table <- DT::renderDataTable({ req(step_results_data()); datatable(step_results_data(), options = list(dom = 't', scrollX = TRUE)) })
  
  output$stepwise_best_header_ui <- renderUI({
    req(best_method_name())
    div(class = "alert alert-success", style = "text-align: center; font-weight: bold; font-size: 1.1em;",
        paste("MODELO VENCEDOR: ", toupper(best_method_name())))
  })
  
  # --- AJUSTE: RESUMO COM ERROS ROBUSTOS ---
  output$stepwise_best_summary <- renderPrint({ 
    req(best_step_model())
    
    cat("--- Resumo Padrão (Sem Correção de Painel) ---\n")
    print(summary(best_step_model()))
    
    cat("\n\n--- AJUSTE ROBUSTO (Clusterizado por Município) ---\n")
    cat("Nota: Este teste corrige a inflação de significância causada pela autocorrelação.\n")
    
    tryCatch({
      # Tenta clusterizar
      coeftest(best_step_model(), vcov=vcovHC(best_step_model(), type="HC1", cluster="group"))
    }, error=function(e) {
      cat("Não foi possível calcular o erro robusto (possível singularidade ou poucos graus de liberdade).")
    })
  })
  
  output$step_resid_plot <- renderPlot({
    req(best_step_model())
    mod <- best_step_model()
    df_r <- data.frame(Fitted = as.numeric(fitted(mod)), Resid = as.numeric(resid(mod)))
    ggplot(df_r, aes(x=Fitted, y=Resid)) + 
      geom_point(alpha=0.5, color="#2c3e50") +
      geom_hline(yintercept=0, color="red", linetype="dashed") +
      labs(title="Resíduos vs Valores Ajustados", subtitle="Verificação de Homocedasticidade", x="Ajustado", y="Resíduo") + theme_minimal()
  })
  
  output$step_qq_plot <- renderPlot({
    req(best_step_model())
    res <- as.numeric(resid(best_step_model()))
    ggplot(data.frame(R=res), aes(sample=R)) + stat_qq(color="#18bc9c") + stat_qq_line() +
      labs(title="Q-Q Plot (Normalidade)", subtitle="Os pontos devem seguir a linha") + theme_minimal()
  })
  
  output$step_resid_tests <- renderPrint({
    req(best_step_model())
    mod <- best_step_model()
    res <- as.numeric(resid(mod))
    
    cat("--- Testes de Resíduos ---\n")
    if(length(res) < 5000) {
      sw <- shapiro.test(res)
      cat("Normalidade (Shapiro-Wilk): P =", format.pval(sw$p.value, digits=4), "\n")
    }
    
    tryCatch({
      ar <- pwartest(mod)
      cat("Autocorrelação Serial (Wooldridge): P =", format.pval(ar$p.value, digits=4), "\n")
      if(ar$p.value > 0.05) cat(">> Não rejeita H0: Sem evidência de autocorrelação (White Noise).\n")
      else cat(">> Rejeita H0: Existe autocorrelação serial.\n")
    }, error = function(e) cat("Não foi possível calcular teste de autocorrelação.\n"))
  })
  
  # 3. LASSO
  output$method_desc <- renderText({
    if(input$sel_method == "LASSO") return("LASSO: Zera coeficientes irrelevantes (Usa lambda.1se para evitar overfitting).")
    if(input$sel_method == "Elastic Net") return("Elastic Net: Combina LASSO e Ridge.")
    return("Stability Selection: Robustez via reamostragem.")
  })
  
  lasso_results <- eventReactive(input$btn_run_lasso, {
    req(input$lasso_dep, input$lasso_idvs)
    
    df_prep <- data_filtered() %>% 
      dplyr::select(Municipio, Ano_Ref, all_of(c(input$lasso_dep, input$lasso_idvs))) %>% 
      na.omit()
    
    if(nrow(df_prep) < 50) {
      showNotification("Amostra muito pequena para LASSO (< 50).", type = "warning")
      return(NULL)
    }
    
    if(input$lasso_fe) {
      df_prep <- df_prep %>%
        group_by(Municipio) %>%
        mutate(across(all_of(c(input$lasso_dep, input$lasso_idvs)), ~ scale(., scale=FALSE))) %>%
        ungroup()
    }
    
    df_model <- df_prep %>% dplyr::select(-Municipio, -Ano_Ref)
    X <- as.matrix(df_model %>% dplyr::select(-all_of(input$lasso_dep)))
    y <- as.vector(df_model[[input$lasso_dep]])
    
    vars_var <- apply(X, 2, var, na.rm=TRUE)
    X <- X[, vars_var > quantile(vars_var, 0.1), drop = FALSE]
    if(ncol(X) < 2) {
      showNotification("Muitas variáveis constantes ou sem variância.", type="warning")
      return(NULL)
    }
    
    X_scaled <- scale(X)
    
    if(input$sel_method %in% c("LASSO", "Elastic Net")) {
      alpha_val <- if(input$sel_method == "LASSO") 1 else input$el_alpha
      fit <- cv.glmnet(X_scaled, y, alpha = alpha_val, nfolds=10)
      c_mat <- as.matrix(coef(fit, s="lambda.1se"))
      df_r <- data.frame(Variavel=rownames(c_mat), Coef=c_mat[,1]) %>% 
        filter(Variavel != "(Intercept)", Coef != 0) %>% arrange(desc(abs(Coef)))
      return(list(type="standard", model=fit, res=df_r))
    } else {
      n_iter <- input$stab_iter; n_sub <- floor(0.5 * nrow(X))
      counts <- rep(0, ncol(X)); names(counts) <- colnames(X)
      withProgress(message="Stability Selection...", {
        for(i in 1:n_iter) {
          idx <- sample(seq_len(nrow(X)), n_sub)
          try({
            ft <- cv.glmnet(X_scaled[idx,], y[idx], alpha=1)
            cf <- as.matrix(coef(ft, s="lambda.min"))
            sel <- rownames(cf)[cf[,1]!=0]
            sel <- setdiff(sel, "(Intercept)")
            counts[sel] <- counts[sel] + 1
          }, silent=T)
          incProgress(1/n_iter)
        }
      })
      freqs <- counts/n_iter
      df_s <- data.frame(Variavel=names(freqs), Freq=freqs) %>% 
        filter(Freq >= input$stab_thresh) %>% arrange(desc(Freq))
      return(list(type="stability", res=df_s, all_freqs=freqs, thresh=input$stab_thresh))
    }
  })
  
  output$lasso_plot <- renderPlot({
    res <- lasso_results()
    req(res)
    if(res$type == "standard") {
      par(mfrow=c(1,2)); plot(res$model); title("CV Error (Lambda.1se)", line=2.5)
      if(nrow(res$res)>0) barplot(height=sort(abs(res$res$Coef)), names.arg=res$res$Variavel[order(abs(res$res$Coef))], horiz=T, las=1, col="#2c3e50")
      par(mfrow=c(1,1))
    } else {
      df_p <- data.frame(Var = names(res$all_freqs), Freq = res$all_freqs)
      df_p$Selected <- df_p$Freq >= res$thresh
      ggplot(df_p, aes(x=reorder(Var, Freq), y=Freq, fill=Selected)) + geom_col() + coord_flip() +
        geom_hline(yintercept=res$thresh, linetype="dashed", color="red") +
        scale_fill_manual(values=c("TRUE"="#18bc9c", "FALSE"="#bdc3c7")) + theme_minimal()
    }
  })
  
  output$lasso_coefs_table <- renderTable({
    res <- lasso_results(); req(res)
    if(res$type == "standard") res$res else res$res %>% mutate(Freq=scales::percent(Freq))
  })
  
  # 4. Regressão Manual (COM FALLBACK PARA SINGULARIDADE)
  output$regression_table_manual <- renderUI({
    req(input$manual_dep, input$manual_idvs, data_filtered())
    cols <- unique(c("Ano_Ref", input$manual_cat_fe, "Natureza_Juridica", input$manual_dep, input$manual_idvs))
    df_base <- data_filtered() %>% dplyr::select(all_of(cols)) %>% na.omit()
    
    run_mod <- function(d) {
      if(nrow(d) < 10) return(NULL)
      pdata <- pdata.frame(d, index = c(input$manual_cat_fe, "Ano_Ref"))
      f <- as.formula(paste(input$manual_dep, "~", paste(input$manual_idvs, collapse = "+")))
      tryCatch(plm(f, pdata, model=switch(input$model_type_manual, "Pooling (OLS)"="pooling", "Fixed Effects (Within)"="within", "Random Effects"="random")), error=function(e) NULL)
    }
    
    models <- list(Geral = run_mod(df_base))
    for(nat in unique(df_base$Natureza_Juridica)) {
      models[[nat]] <- run_mod(df_base %>% filter(Natureza_Juridica == nat))
    }
    models <- Filter(Negate(is.null), models)
    
    if(length(models) == 0) return(HTML("Erro na estimativa."))
    
    get_robust_se <- function(m) {
      if(input$manual_se_type == "Standard") return(NULL)
      
      se <- tryCatch({
        if(input$manual_se_type == "Clustered") sqrt(diag(vcovHC(m, type="HC1")))
        else if(input$manual_se_type == "Driscoll-Kraay") sqrt(diag(vcovSCC(m)))
        else NULL
      }, error = function(e) return(NULL))
      
      return(se)
    }
    
    se_list <- lapply(models, get_robust_se)
    
    note_txt <- paste("Erro Padrão:", input$manual_se_type)
    if(any(sapply(se_list, is.null)) && input$manual_se_type != "Standard") {
      note_txt <- paste(note_txt, "(Alguns modelos usaram erro padrão simples devido à singularidade).")
    }
    
    HTML(stargazer(models, type="html", se=se_list, header=FALSE, column.labels=names(models), notes=note_txt, notes.append=TRUE))
  })
  
  # 5. DIAGNÓSTICOS PÓS-FE (BLINDADO)
  output$diag_results_ui <- renderUI({
    input$btn_run_diag
    isolate({
      req(input$diag_dep, input$diag_idvs)
      
      df <- data_filtered() %>% dplyr::select(Municipio, Ano_Ref, all_of(input$diag_dep), all_of(input$diag_idvs)) %>% na.omit()
      if(nrow(df) < 10 || length(unique(df$Municipio)) < 2) return(div(class="alert alert-warning", "Dados insuficientes."))
      pdata <- pdata.frame(df, index = c("Municipio", "Ano_Ref"))
      f <- as.formula(paste(input$diag_dep, "~", paste(input$diag_idvs, collapse = "+")))
      
      tryCatch({
        mod <- plm(f, pdata, model="within")
        
        safe_test <- function(expr) tryCatch(expr, error=function(e) list(p.value=NA), warning=function(w) list(p.value=NA))
        ar <- safe_test(pwartest(mod))
        bp <- safe_test(bptest(mod))
        cd <- safe_test(pcdtest(mod))
        
        get_col <- function(p) if(is.na(p)) "warning" else if(p < 0.05) "danger" else "success"
        get_txt <- function(p, type) {
          if(is.na(p)) return("Inconclusivo")
          if(p < 0.05) return(paste("Rejeita H0:", switch(type, "ar"="Autocorr.", "bp"="Heteroced.", "cd"="Dependência")))
          return("Aceita H0 (OK)")
        }
        
        rec_text <- "Modelo FE padrão parece adequado."
        if((!is.na(ar$p.value) && ar$p.value < 0.05) || (!is.na(bp$p.value) && bp$p.value < 0.05)) rec_text <- "Sugerido: Erros Padrão Robustos Clusterizados (vcovHC)."
        if(!is.na(cd$p.value) && cd$p.value < 0.05) rec_text <- "Sugerido: Estimador de Driscoll-Kraay (vcovSCC)."
        if(is.na(cd$p.value)) rec_text <- paste(rec_text, "(Aviso: Testes de dependência falharam por falta de dados cruzados).")
        
        fluidRow(
          column(4, div(class=paste0("card text-white bg-", get_col(bp$p.value), " mb-3"),
                        div(class="card-header", "Heterocedasticidade"),
                        div(class="card-body", h5("Breusch-Pagan"), p(fmt_pval(bp$p.value)), tags$small(get_txt(bp$p.value, "bp"))))),
          column(4, div(class=paste0("card text-white bg-", get_col(cd$p.value), " mb-3"),
                        div(class="card-header", "Dependência Seccional"),
                        div(class="card-body", h5("Pesaran CD"), p(fmt_pval(cd$p.value)), tags$small(get_txt(cd$p.value, "cd"))))),
          column(4, div(class=paste0("card text-white bg-", get_col(ar$p.value), " mb-3"),
                        div(class="card-header", "Autocorrelação"),
                        div(class="card-body", h5("Wooldridge AR(1)"), p(fmt_pval(ar$p.value)), tags$small(get_txt(ar$p.value, "ar"))))),
          column(12, div(class="alert alert-info", style="text-align: center; font-weight: bold;", paste("Recomendação Final:", rec_text)))
        )
      }, error = function(e) div(class="alert alert-danger", e$message))
    })
  })
  
  # 6. VIF (CORRIGIDO)
  output$vif_diagnosis <- renderText({
    req(input$vif_idvs)
    dep_var <- if(!is.null(input$manual_dep)) input$manual_dep else names(values$raw_df)[grepl("es001", names(values$raw_df), ignore.case=T)][1]
    
    df <- data_filtered() %>% dplyr::select(all_of(c(dep_var, input$vif_idvs))) %>% na.omit()
    
    if(nrow(df) < length(input$vif_idvs)) return("Dados insuficientes para cálculo do VIF.")
    
    # Verifica alias (colinearidade perfeita)
    model_vif <- lm(as.formula(paste(dep_var, "~", paste(input$vif_idvs, collapse = "+"))), data = df)
    
    if(any(is.na(coef(model_vif)))) return("ERRO: Colinearidade Perfeita (Aliasing). Remova variáveis redundantes.")
    
    tryCatch({
      vif_vals <- car::vif(model_vif)
      if(is.matrix(vif_vals)) vif_vals <- vif_vals[,1] # Handle GVIF
      
      if(any(vif_vals > 10)) {
        high <- names(vif_vals)[vif_vals > 10]
        paste("ALERTA: Multicolinearidade crítica detectada (>10) em:", paste(high, collapse=", "))
      } else if(any(vif_vals > 5)) {
        med <- names(vif_vals)[vif_vals > 5]
        paste("AVISO: Multicolinearidade moderada (>5) em:", paste(med, collapse=", "))
      } else {
        "Diagnóstico VIF: Multicolinearidade sob controle (Todos < 5)."
      }
    }, error = function(e) "Erro no cálculo do VIF.")
  })
  
  output$vif_result_table <- renderTable({
    input$btn_calc_vif
    isolate({
      req(input$vif_idvs)
      dep_var <- if(!is.null(input$manual_dep)) input$manual_dep else names(values$raw_df)[grepl("es001", names(values$raw_df), ignore.case=T)][1]
      
      df <- data_filtered() %>% dplyr::select(all_of(c(dep_var, input$vif_idvs))) %>% na.omit()
      model_vif <- lm(as.formula(paste(dep_var, "~", paste(input$vif_idvs, collapse = "+"))), data = df)
      
      v <- car::vif(model_vif)
      if(is.matrix(v)) v <- v[,1]
      data.frame(Variavel=names(v), VIF=v)
    })
  })
}

shinyApp(ui, server)

