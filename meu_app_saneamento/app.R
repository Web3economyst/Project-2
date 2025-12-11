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

# ==============================================================================
# TEMA
# ==============================================================================
my_theme <- bs_theme(
  version = 5,
  bootswatch = "zephyr",
  primary = "#2c3e50",
  secondary = "#18bc9c",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat")
)

# ==============================================================================
# FUNÇÕES AUXILIARES
# ==============================================================================
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

get_plm_info <- function(plm_model) {
  rss <- sum(residuals(plm_model)^2)
  n <- length(residuals(plm_model))
  k <- length(coef(plm_model))
  aic_val <- 2 * k + n * log(rss / n)
  bic_val <- log(n) * k + n * log(rss / n)
  return(c(AIC = aic_val, BIC = bic_val))
}

clean_pt_num <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.na(x) || x == "" || x == "-") return(NA)
  x_clean <- gsub("\\.", "", x)
  x_clean <- gsub(",", ".", x_clean)
  val <- suppressWarnings(as.numeric(x_clean))
  if(is.infinite(val)) return(NA)
  return(val)
}

# ==============================================================================
# CONFIGURAÇÃO
# ==============================================================================
GITHUB_CSV_URL <- "https://github.com/Web3economyst/Project-2/raw/refs/heads/main/database_saneamento.csv"

# Lista de Códigos Padrão
default_codes_list <- unique(c(
  "FN002", "FN007", "FN004", "FN006", "FN008", "FN010", "FN011", "FN013", 
  "FN014", "FN027", "FN037", "FN016", "FN020", "FN021", "FN022", "FN034", 
  "FN023", "FN001", "FN042", "FN052", "AG006", "AG028",
  "FN003", "FN024", "FN043", "FN053", "ES001"
))

# ==============================================================================
# UI
# ==============================================================================
ui <- page_sidebar(
  theme = my_theme,
  title = "Dashboard Saneamento RS (Full Data)",
  
  sidebar = sidebar(
    title = "Filtros Globais",
    class = "bg-light",
    p("Filtre a base inteira."),
    hr(),
    selectInput("global_natureza_juridica", "Natureza Jurídica:", choices = NULL, multiple = TRUE, selectize = TRUE),
    selectInput("global_estado", "Estado (UF):", choices = NULL, multiple = TRUE, selectize = TRUE),
    selectInput("global_municipio", "Município:", choices = NULL, multiple = TRUE, selectize = TRUE),
    
    uiOutput("ui_year_slider"),
    helpText("Use o slider para restringir o período."),
    hr(),
    div(class = "alert alert-info", style = "font-size: 0.8em;",
        "Dica: As variáveis mantêm seus códigos originais (ex: fn002...).")
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
                            selectizeInput("miss_var_select", "Selecione as Variáveis:", choices = NULL, multiple = TRUE, options = list(placeholder = "Padrão: Seleção Personalizada")),
                            helpText("Por padrão, exibe as variáveis FN/AG solicitadas.")
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
                          sidebarPanel(width = 3, 
                                       selectInput("var_hist", "Variável:", choices = NULL) 
                          ),
                          mainPanel(plotOutput("histogram"))
                        )
               ),
               tabPanel("Tendências",
                        sidebarLayout(
                          sidebarPanel(width = 3, 
                                       selectInput("var_trend", "Variável Y:", choices = NULL) 
                          ),
                          mainPanel(plotOutput("trend_graph"))
                        )
               ),
               
               # --- ABA CORRELAÇÕES ---
               tabPanel("Correlações",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3, 
                            h5("Seleção de Variáveis"),
                            p("Adicione aqui todas as variáveis que deseja analisar (várias juntas)."),
                            selectInput("var_corr_vars", "Vars Matriz / Barras:", choices = NULL, multiple = TRUE), 
                            hr(),
                            h5("Variável Base (Foco)"),
                            p("Selecione a variável central para o gráfico de barras."),
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
               
               tabPanel("Comparador (Auto)", 
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            h4("Configuração"),
                            selectInput("comp_dep", "Variável Dependente (Y):", choices = NULL),
                            selectInput("comp_idvs", "Candidatas a Independentes (X):", choices = NULL, multiple = TRUE),
                            hr(),
                            actionButton("btn_run_comparison", "Rodar Comparativo", class = "btn-primary btn-lg", width = "100%")
                          ),
                          mainPanel(
                            h3("Resultados (Stepwise + Painel)"),
                            p("Nota: O comparador usa 'Município' como índice padrão."),
                            DT::dataTableOutput("comparison_table"),
                            br(),
                            verbatimTextOutput("best_model_summary")
                          )
                        )
               ),
               
               tabPanel("Regressão Manual",
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            selectInput("model_type_manual", "Tipo de Modelo:", c("Pooling (OLS)", "Fixed Effects (Within)", "Random Effects")),
                            hr(),
                            selectInput("manual_cat_fe", "1º Efeito Fixo (Categorical Variable):", choices = NULL, selected = "municipio"),
                            helpText("Define a variável de agrupamento (Index Individual)."),
                            selectInput("manual_se_type", "Standard Errors:", c("Standard", "Clustered")),
                            helpText("Clustered agrupa erros pelo 1º Efeito Fixo."),
                            hr(),
                            selectInput("manual_dep", "Dependente:", choices = NULL),
                            selectInput("manual_idvs", "Independentes:", choices = NULL, multiple = TRUE)
                          ),
                          mainPanel(
                            htmlOutput("regression_table_manual"),
                            htmlOutput("manual_diagnostics")
                          )
                        )
               ),
               
               # --- ABA DIAGNÓSTICOS PÓS-FE ---
               tabPanel("Diagnósticos (Pós-FE)",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            h4("Diagnósticos de Resíduos"),
                            p("Testes essenciais para validar o modelo de Efeitos Fixos (Within)."),
                            hr(),
                            selectInput("diag_cat_fe", "Efeito Fixo (ID):", choices = NULL, selected = "municipio"),
                            selectInput("diag_dep", "Dependente (Y):", choices = NULL),
                            selectInput("diag_idvs", "Independentes (X):", choices = NULL, multiple = TRUE),
                            hr(),
                            actionButton("btn_run_diag", "Rodar Diagnósticos", class = "btn-warning", width = "100%")
                          ),
                          mainPanel(
                            h3("Diagnósticos Essenciais Após o FE"),
                            hr(),
                            uiOutput("diag_results_ui")
                          )
                        )
               ),
               
               # --- ABA VIF ---
               tabPanel("Teste de Multicolinearidade (VIF)",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            h4("Configuração VIF"),
                            p("Mede a inflação da variância devido à colinearidade."),
                            selectInput("vif_idvs", "Variáveis Independentes (X):", choices = NULL, multiple = TRUE),
                            helpText("Selecione variáveis que não sejam soma uma da outra."),
                            hr(),
                            actionButton("btn_calc_vif", "Calcular VIF", class = "btn-info", width = "100%")
                          ),
                          mainPanel(
                            h3("Fatores de Inflação da Variância (VIF)"),
                            div(class = "alert alert-info", 
                                "Interpretação: VIF > 10 indica multicolinearidade severa."),
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
  comparison_results <- reactiveVal(NULL)
  best_model_obj <- reactiveVal(NULL)
  
  # --- 1. CARREGAMENTO E PROCESSAMENTO ---
  observe({
    req(is.null(values$raw_df))
    
    id <- showNotification("Carregando base...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    raw <- tryCatch({
      read.csv(GITHUB_CSV_URL, stringsAsFactors = FALSE, fileEncoding = "latin1", check.names = FALSE)
    }, error = function(e) {
      tryCatch({
        read.csv(GITHUB_CSV_URL, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE)
      }, error = function(e2) {
        read.csv(GITHUB_CSV_URL, stringsAsFactors = FALSE, check.names = FALSE)
      })
    })
    
    clean <- raw %>% janitor::clean_names()
    
    # Renomeação ESTRUTURAL
    if("municipio" %in% names(clean)) names(clean)[names(clean) == "municipio"] <- "Municipio"
    if("estado" %in% names(clean)) names(clean)[names(clean) == "estado"] <- "Sigla_UF"
    if("ano_de_referencia" %in% names(clean)) names(clean)[names(clean) == "ano_de_referencia"] <- "Ano_Ref"
    if("natureza_juridica" %in% names(clean)) names(clean)[names(clean) == "natureza_juridica"] <- "Natureza_Juridica"
    
    cols_all <- names(clean)
    cols_skip <- c("Municipio", "Sigla_UF", "Natureza_Juridica", "prestador", "sigla_do_prestador", "abrangencia", "tipo_de_servico")
    cols_to_convert <- setdiff(cols_all, cols_skip)
    
    for(col in cols_to_convert) {
      if(is.character(clean[[col]])) {
        clean[[col]] <- sapply(clean[[col]], clean_pt_num)
      }
    }
    
    # --- CRIAÇÃO AUTOMÁTICA DA VARIÁVEL LUCRO_OP ---
    col_rev <- names(clean)[grepl("^fn002", names(clean))][1]
    col_exp <- names(clean)[grepl("^fn015", names(clean))][1]
    if(!is.na(col_rev) && !is.na(col_exp)) {
      clean$Lucro_op <- clean[[col_rev]] - clean[[col_exp]]
    }
    
    # --- CRIAÇÃO AUTOMÁTICA DA VARIÁVEL DESP_PPE (NOVA) ---
    # desp_ppe = fn011 + fn010 + fn013
    col_fn011 <- names(clean)[grepl("^fn011", names(clean))][1]
    col_fn010 <- names(clean)[grepl("^fn010", names(clean))][1]
    col_fn013 <- names(clean)[grepl("^fn013", names(clean))][1]
    
    if(!is.na(col_fn011) && !is.na(col_fn010) && !is.na(col_fn013)) {
      clean$desp_ppe <- clean[[col_fn011]] + clean[[col_fn010]] + clean[[col_fn013]]
    }
    # -----------------------------------------------------
    
    values$raw_df <- clean
    values$col_names <- names(clean)
    
    if("Natureza_Juridica" %in% names(clean)) {
      opcoes <- sort(unique(clean$Natureza_Juridica))
      opcoes <- opcoes[opcoes != "" & !is.na(opcoes)]
      updateSelectInput(session, "global_natureza_juridica", choices = opcoes)
    }
    if("Sigla_UF" %in% names(clean)) {
      ufs <- sort(unique(clean$Sigla_UF))
      ufs <- ufs[ufs != "" & !is.na(ufs)]
      updateSelectInput(session, "global_estado", choices = ufs)
    }
    if("Municipio" %in% names(clean)) {
      munis <- sort(unique(clean$Municipio))
      updateSelectInput(session, "global_municipio", choices = munis)
    }
    
    char_cols <- names(clean)[sapply(clean, function(x) is.character(x) || is.factor(x))]
    sel_def <- "Municipio"
    if(!sel_def %in% char_cols) sel_def <- char_cols[1]
    updateSelectInput(session, "manual_cat_fe", choices = char_cols, selected = sel_def)
    updateSelectInput(session, "diag_cat_fe", choices = char_cols, selected = sel_def)
  })
  
  # --- UI UPDATES DINÂMICOS ---
  observeEvent(values$col_names, {
    req(values$raw_df)
    clean <- values$raw_df
    
    all_clean_names <- names(clean)
    defaults_selected <- c()
    if("Lucro_op" %in% all_clean_names) defaults_selected <- c("Lucro_op")
    if("desp_ppe" %in% all_clean_names) defaults_selected <- c(defaults_selected, "desp_ppe")
    
    for(code in default_codes_list) {
      code_clean <- tolower(code)
      matches <- all_clean_names[grepl(paste0("^", code_clean), all_clean_names)]
      if(length(matches) > 0) {
        defaults_selected <- c(defaults_selected, matches[1])
      }
    }
    defaults_selected <- unique(defaults_selected)
    
    nums <- names(clean)[sapply(clean, is.numeric)]
    valid_defaults <- intersect(defaults_selected, nums)
    if(length(valid_defaults) == 0) valid_defaults <- nums[1:min(5, length(nums))]
    
    def_dep <- if("Lucro_op" %in% nums) "Lucro_op" else nums[1]
    
    safe_select <- function(current_val, default_val) {
      if (!is.null(current_val) && current_val %in% nums) return(current_val)
      return(default_val)
    }
    safe_select_multi <- function(current_vals, default_vals) {
      if (!is.null(current_vals)) {
        valid_vals <- intersect(current_vals, nums)
        if (length(valid_vals) > 0) return(valid_vals)
      }
      return(default_vals)
    }
    
    updateSelectizeInput(session, "miss_var_select", choices = nums, selected = safe_select_multi(input$miss_var_select, valid_defaults), server = TRUE)
    updateSelectInput(session, "comp_idvs", choices = nums, selected = safe_select_multi(input$comp_idvs, valid_defaults[1:min(length(valid_defaults), 10)]))
    updateSelectInput(session, "manual_idvs", choices = nums, selected = safe_select_multi(input$manual_idvs, valid_defaults[1:min(length(valid_defaults), 5)]))
    updateSelectInput(session, "diag_idvs", choices = nums, selected = safe_select_multi(input$diag_idvs, valid_defaults[1:min(length(valid_defaults), 5)]))
    updateSelectInput(session, "var_corr_vars", choices = nums, selected = safe_select_multi(input$var_corr_vars, valid_defaults)) 
    updateSelectInput(session, "vif_idvs", choices = nums, selected = safe_select_multi(input$vif_idvs, valid_defaults[1:min(length(valid_defaults), 5)]))
    
    updateSelectInput(session, "comp_dep", choices = nums, selected = safe_select(input$comp_dep, def_dep))
    updateSelectInput(session, "manual_dep", choices = nums, selected = safe_select(input$manual_dep, def_dep))
    updateSelectInput(session, "diag_dep", choices = nums, selected = safe_select(input$diag_dep, def_dep))
    updateSelectInput(session, "var_hist", choices = nums, selected = safe_select(input$var_hist, def_dep))
    updateSelectInput(session, "var_trend", choices = nums, selected = safe_select(input$var_trend, def_dep))
    updateSelectInput(session, "corr_focus_var", choices = nums, selected = safe_select(input$corr_focus_var, valid_defaults[1]))
  })
  
  observeEvent(input$global_estado, ignoreNULL = FALSE, {
    req(values$raw_df)
    df <- values$raw_df
    if(!is.null(input$global_estado)) {
      df_filt <- df %>% filter(Sigla_UF %in% input$global_estado)
      munis_disponiveis <- sort(unique(df_filt$Municipio))
    } else {
      munis_disponiveis <- sort(unique(df$Municipio))
    }
    updateSelectInput(session, "global_municipio", choices = munis_disponiveis, selected = intersect(input$global_municipio, munis_disponiveis))
  })
  
  output$ui_year_slider <- renderUI({
    req(values$raw_df)
    if("Ano_Ref" %in% names(values$raw_df)) {
      anos <- na.omit(values$raw_df$Ano_Ref)
      min_ano <- min(anos)
      max_ano <- max(anos)
      sliderInput("global_ano_range", "Período (Anos):", min = min_ano, max = max_ano, value = c(min_ano, max_ano), step = 1, sep = "")
    } else {
      p("Coluna 'Ano_Ref' não encontrada.")
    }
  })
  
  data_filtered <- reactive({
    req(values$raw_df)
    df <- values$raw_df
    if (!is.null(input$global_natureza_juridica)) if("Natureza_Juridica" %in% names(df)) df <- df %>% filter(Natureza_Juridica %in% input$global_natureza_juridica)
    if (!is.null(input$global_estado)) if("Sigla_UF" %in% names(df)) df <- df %>% filter(Sigla_UF %in% input$global_estado)
    if (!is.null(input$global_municipio)) if("Municipio" %in% names(df)) df <- df %>% filter(Municipio %in% input$global_municipio)
    if (!is.null(input$global_ano_range) && "Ano_Ref" %in% names(df)) df <- df %>% filter(Ano_Ref >= input$global_ano_range[1] & Ano_Ref <= input$global_ano_range[2])
    df
  })
  
  observeEvent(input$btn_create_var, {
    req(input$new_var_name, input$new_var_formula, values$raw_df)
    tryCatch({
      new_data <- values$raw_df %>%
        mutate(!!sym(input$new_var_name) := !!parse_expr(input$new_var_formula))
      if(sum(is.infinite(new_data[[input$new_var_name]])) > 0) showNotification("Aviso: Infinitos gerados.", type = "warning")
      values$raw_df <- new_data
      values$col_names <- names(new_data)
      showNotification(paste("Variável", input$new_var_name, "criada!"), type = "message")
    }, error = function(e) showNotification(paste("Erro:", e$message), type = "error"))
  })
  
  # --- OUTPUTS ---
  output$list_vars_available <- renderPrint({ req(values$col_names); print(values$col_names) })
  output$full_data <- DT::renderDataTable({ req(data_filtered()); data_filtered() }, options = list(scrollX = TRUE, pageLength = 10))
  
  output$missing_values <- renderPlot({
    req(data_filtered())
    df <- data_filtered()
    if(nrow(df) == 0) return(NULL)
    validate(need(!is.null(input$miss_var_select), "Carregando variáveis..."))
    vars_plot <- input$miss_var_select
    if(length(vars_plot) == 0) return(NULL)
    df_na <- df %>% dplyr::select(any_of(vars_plot), Ano_Ref)
    if ("Ano_Ref" %in% names(df_na)) {
      df_na_long <- df_na %>% group_by(Ano_Ref) %>% summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>% pivot_longer(-Ano_Ref, names_to = "Variavel", values_to = "Porcentagem_NA")
      ggplot(df_na_long, aes(x = as.factor(Ano_Ref), y = Variavel, fill = Porcentagem_NA)) +
        geom_tile(color = "white") + geom_text(aes(label = round(Porcentagem_NA, 1)), color = ifelse(df_na_long$Porcentagem_NA > 50, "white", "black"), size = 3) +
        scale_fill_gradient(low = "#d4edda", high = "#e74c3c", limits = c(0, 100)) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Heatmap de Missings (% NA)", x = "Ano", y = NULL)
    }
  })
  
  output$bar_chart <- renderPlot({
    req(data_filtered())
    if ("Sigla_UF" %in% names(data_filtered())) {
      data_filtered() %>% count(Sigla_UF) %>% ggplot(aes(x = reorder(Sigla_UF, -n), y = n)) + geom_col(fill = "#2c3e50") + theme_minimal() + labs(title = "Observações por UF", x = "Estado", y = "Contagem")
    }
  })
  
  output$descriptive_stats <- renderUI({
    req(data_filtered())
    df_desc <- data_filtered() %>% dplyr::select(where(is.numeric))
    if(ncol(df_desc) > 0 && nrow(df_desc) > 0) {
      tryCatch({ HTML(prepare_descriptive_table(df_desc)$kable_ret) }, error = function(e) HTML("Erro ao gerar tabela."))
    } else { HTML("Sem dados numéricos.") }
  })
  
  output$histogram <- renderPlot({
    req(data_filtered())
    validate(need(input$var_hist, "Carregando..."), need(input$var_hist %in% names(data_filtered()), "Variável não encontrada."))
    val <- as.numeric(data_filtered()[[input$var_hist]])
    hist(val, col = "#18bc9c", border = "white", main = paste("Distr.", input$var_hist), xlab = input$var_hist, breaks = 30)
  })
  
  output$trend_graph <- renderPlot({
    req(data_filtered())
    validate(need(input$var_trend, "Carregando..."), need(input$var_trend %in% names(data_filtered()), "Variável não encontrada."))
    df <- data_filtered()
    if("Ano_Ref" %in% names(df)) {
      df %>% group_by(Ano_Ref) %>% summarise(M = mean(.data[[input$var_trend]], na.rm=T)) %>%
        ggplot(aes(x=Ano_Ref, y=M)) + geom_line(color="#2c3e50", linewidth=1.2) + geom_point(size=3, color="#18bc9c") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Média Anual", y = input$var_trend)
    }
  })
  
  output$correlation_plot <- renderPlotly({
    req(data_filtered())
    validate(need(length(input$var_corr_vars) > 1, "Selecione pelo menos 2 variáveis."))
    df_sel <- data_filtered() %>% dplyr::select(any_of(input$var_corr_vars)) %>% dplyr::select(where(is.numeric)) %>% na.omit()
    if(ncol(df_sel) < 2 || nrow(df_sel) < 5) return(NULL)
    cor_mat <- cor(df_sel)
    p_mat <- cor_pvalue_matrix(df_sel)
    cor_df <- as.data.frame(cor_mat) %>% tibble::rownames_to_column("Var1") %>% pivot_longer(-Var1, names_to = "Var2", values_to = "Cor")
    p_df <- as.data.frame(p_mat) %>% tibble::rownames_to_column("Var1") %>% pivot_longer(-Var1, names_to = "Var2", values_to = "Pval")
    plot_data <- left_join(cor_df, p_df, by = c("Var1", "Var2")) %>% mutate(Txt = paste0("R: ", round(Cor, 3), "<br>P: ", scales::scientific(Pval)))
    gg <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = Cor, text = Txt)) + geom_tile(color = "white") + scale_fill_gradient2(limit = c(-1, 1), low="#e74c3c", mid="white", high="#2c3e50") + theme_minimal() + coord_fixed() + theme(axis.text.x = element_text(angle=45, hjust=1))
    ggplotly(gg, tooltip = "text")
  })
  
  output$correlation_bar_plot <- renderPlotly({
    req(data_filtered())
    df <- data_filtered()
    df_num <- df %>% dplyr::select(where(is.numeric))
    validate(need(input$corr_focus_var, "Carregando..."), need(input$corr_focus_var %in% names(df_num), "Variável foco não encontrada."))
    if(var(df_num[[input$corr_focus_var]], na.rm = TRUE) == 0) return(plotly_empty() %>% layout(title = "Foco constante."))
    vars_to_compare <- input$var_corr_vars
    if(is.null(vars_to_compare)) vars_to_compare <- names(df_num)
    vars_to_compare <- setdiff(vars_to_compare, input$corr_focus_var)
    vars_to_compare <- intersect(vars_to_compare, names(df_num))
    if(length(vars_to_compare) == 0) return(NULL)
    cor_vals <- numeric(); var_names <- character()
    for(v in vars_to_compare) {
      if(var(df_num[[v]], na.rm=TRUE) > 0) {
        val <- cor(df_num[[input$corr_focus_var]], df_num[[v]], use = "pairwise.complete.obs")
        if(!is.na(val)) { cor_vals <- c(cor_vals, val); var_names <- c(var_names, v) }
      }
    }
    if(length(cor_vals) == 0) return(plotly_empty())
    plot_data <- data.frame(Variavel = var_names, Correlacao = cor_vals) %>% arrange(desc(Correlacao))
    if(nrow(plot_data) > 30) plot_data <- plot_data %>% mutate(abs_corr = abs(Correlacao)) %>% slice_max(abs_corr, n = 30)
    gg <- ggplot(plot_data, aes(x = reorder(Variavel, Correlacao), y = Correlacao, fill = Correlacao)) + geom_col() + coord_flip() + scale_fill_gradient2(low = "#e74c3c", mid = "white", high = "#18bc9c", limits = c(-1, 1)) + theme_minimal() + labs(title = paste("Correlação com:", input$corr_focus_var), x = NULL, y = "Coeficiente de Pearson")
    ggplotly(gg)
  })
  
  output$regression_table_manual <- renderUI({
    req(input$manual_dep, input$manual_idvs, input$manual_cat_fe, data_filtered())
    cols_to_use <- unique(c("Ano_Ref", input$manual_cat_fe, input$manual_dep, input$manual_idvs))
    if(!all(cols_to_use %in% names(data_filtered()))) return(HTML(paste("<div class='alert alert-danger'>Erro: Variáveis não encontradas.</div>")))
    df_reg <- data_filtered() %>% dplyr::select(all_of(cols_to_use))
    df_reg[sapply(df_reg, is.infinite)] <- NA
    df_reg <- na.omit(df_reg)
    df_reg <- df_reg %>% group_by(across(all_of(c(input$manual_cat_fe, "Ano_Ref")))) %>% summarise(across(everything(), sum, na.rm=TRUE), .groups="drop")
    validate(need(nrow(df_reg) > 10, "Dados insuficientes."), need(length(unique(df_reg$Ano_Ref)) > 1, "Necessário mais de 1 ano."), need(length(unique(df_reg[[input$manual_cat_fe]])) > 0, "Categórica vazia."))
    for(var in input$manual_idvs) if(var(df_reg[[var]]) == 0) return(HTML(paste0("<div class='alert alert-warning'>Erro: Variável '", var, "' é constante. Remova-a.</div>")))
    
    pdata <- pdata.frame(df_reg, index = c(input$manual_cat_fe, "Ano_Ref"))
    f <- as.formula(paste(input$manual_dep, "~", paste(input$manual_idvs, collapse = "+")))
    result_html <- tryCatch({
      mod <- switch(input$model_type_manual,
                    "Pooling (OLS)" = plm(f, pdata, model="pooling"),
                    "Fixed Effects (Within)" = plm(f, pdata, model="within"),
                    "Random Effects" = plm(f, pdata, model="random"))
      se_list <- NULL
      if(input$manual_se_type == "Clustered") { cov_clus <- vcovHC(mod, type = "HC1", cluster = "group"); se_list <- list(sqrt(diag(cov_clus))) }
      paste(capture.output(stargazer(mod, type="html", header=FALSE, se = se_list, notes = paste("SE:", input$manual_se_type))), collapse="\n")
    }, error = function(e) paste("<div class='alert alert-danger'>Erro Estimação:<br>", e$message, "</div>"))
    HTML(result_html)
  })
  
  output$manual_diagnostics <- renderUI({
    req(input$manual_dep, data_filtered())
    if(input$model_type_manual == "Pooling (OLS)") return(NULL)
    HTML("<div class='alert alert-warning'>Diagnósticos disponíveis no comparador automático.</div>")
  })
  
  # --- DIAGNÓSTICOS PÓS-FE ---
  diag_results <- eventReactive(input$btn_run_diag, {
    req(input$diag_dep, input$diag_idvs, input$diag_cat_fe, data_filtered())
    cols <- unique(c("Ano_Ref", input$diag_cat_fe, input$diag_dep, input$diag_idvs))
    df_d <- data_filtered() %>% dplyr::select(all_of(cols))
    df_d[sapply(df_d, is.infinite)] <- NA
    df_d <- na.omit(df_d)
    df_d <- df_d %>% group_by(across(all_of(c(input$diag_cat_fe, "Ano_Ref")))) %>% summarise(across(everything(), sum, na.rm=TRUE), .groups="drop")
    if(nrow(df_d) < 10) return(list(error = "Dados insuficientes."))
    pdata <- pdata.frame(df_d, index = c(input$diag_cat_fe, "Ano_Ref"))
    f <- as.formula(paste(input$diag_dep, "~", paste(input$diag_idvs, collapse = "+")))
    tryCatch({
      mod_fe <- plm(f, data = pdata, model = "within")
      mod_pool <- plm(f, data = pdata, model = "pooling")
      
      f_test <- tryCatch(pFtest(mod_fe, mod_pool), error = function(e) list(statistic=NA, p.value=NA, method="Erro no teste F"))
      ar_test <- tryCatch(pwartest(mod_fe), error = function(e) list(statistic=NA, p.value=NA, method="Erro no teste AR"))
      bp_test <- tryCatch(bptest(mod_fe), error = function(e) list(statistic=NA, p.value=NA, method="Erro no teste BP"))
      cd_test <- tryCatch(pcdtest(mod_fe, test = "cd"), error = function(e) list(statistic=NA, p.value=NA, method="Erro no teste CD"))
      
      list(error = NULL, f_test = f_test, ar = ar_test, bp = bp_test, cd = cd_test)
    }, error = function(e) list(error = e$message))
  })
  
  output$diag_results_ui <- renderUI({
    res <- diag_results()
    if(is.null(res)) return(NULL)
    if(!is.null(res$error)) return(HTML(paste("<div class='alert alert-danger'>Erro:", res$error, "</div>")))
    fmt_pval <- function(p) { if(is.na(p)) return("-"); if(p < 0.001) return("< 0.001 ***"); if(p < 0.01) return(paste(round(p,4), "**")); if(p < 0.05) return(paste(round(p,4), "*")); return(round(p,4)) }
    div(
      h4("1. Teste F para Efeitos Individuais (Chow)"), p(paste("Estatística:", round(res$f_test$statistic, 3))), p(paste("P-Valor:", fmt_pval(res$f_test$p.value))), p(if(!is.na(res$f_test$p.value) && res$f_test$p.value < 0.05) "Conclusão: Rejeita H0 (Efeitos Fixos são necessários)." else "Conclusão: Não rejeita H0 (OLS é suficiente)."), hr(),
      h4("2. Autocorrelação (Wooldridge test)"), p(paste("Estatística:", round(res$ar$statistic, 3))), p(paste("P-Valor:", fmt_pval(res$ar$p.value))), p(if(!is.na(res$ar$p.value) && res$ar$p.value < 0.05) "Conclusão: Rejeita H0 (Existe autocorrelação serial)." else "Conclusão: Não rejeita H0 (Sem evidência de autocorrelação)."), hr(),
      h4("3. Heterocedasticidade (Breusch-Pagan test)"), p(paste("Estatística:", round(res$bp$statistic, 3))), p(paste("P-Valor:", fmt_pval(res$bp$p.value))), p(if(!is.na(res$bp$p.value) && res$bp$p.value < 0.05) "Conclusão: Rejeita H0 (Heterocedasticidade presente)." else "Conclusão: Homocedasticidade."), hr(),
      h4("4. Dependência Seccional (Pesaran CD test)"), p(paste("Estatística:", round(res$cd$statistic, 3))), p(paste("P-Valor:", fmt_pval(res$cd$p.value))), p(if(!is.na(res$cd$p.value) && res$cd$p.value < 0.05) "Conclusão: Rejeita H0 (Dependência entre unidades/municípios)." else "Conclusão: Independência seccional.")
    )
  })
  
  observeEvent(input$btn_run_comparison, {
    req(input$comp_dep, input$comp_idvs, data_filtered())
    withProgress(message = 'Rodando Comparativo...', value = 0, {
      cols_use <- c("Municipio", "Ano_Ref", input$comp_dep, input$comp_idvs)
      df_reg <- data_filtered() %>% dplyr::select(all_of(cols_use))
      df_reg[sapply(df_reg, is.infinite)] <- NA
      df_reg <- na.omit(df_reg)
      df_reg <- df_reg %>% group_by(Municipio, Ano_Ref) %>% summarise(across(everything(), sum, na.rm=TRUE), .groups="drop")
      if(nrow(df_reg) < 10) { showNotification("Dados insuficientes.", type="error"); return(NULL) }
      pdata <- pdata.frame(df_reg, index = c("Municipio", "Ano_Ref"))
      form_full <- as.formula(paste(input$comp_dep, "~", paste(input$comp_idvs, collapse = "+")))
      ols_full <- lm(form_full, data = df_reg)
      ols_null <- lm(as.formula(paste(input$comp_dep, "~ 1")), data = df_reg)
      results_list <- list(); counter <- 1
      for (dir in c("forward", "backward", "both")) {
        incProgress(1/4, detail = dir)
        model_start <- if(dir == "backward") ols_full else ols_null
        step_res <- stepAIC(model_start, scope = list(lower=ols_null, upper=ols_full), direction = dir, trace = 0)
        vars_selected <- names(coef(step_res))[-1]
        if(length(vars_selected) == 0) vars_selected <- "1"
        f_final <- as.formula(paste(input$comp_dep, "~", paste(vars_selected, collapse = "+")))
        mod_fe <- tryCatch(plm(f_final, data = pdata, model = "within"), error = function(e) NULL)
        mod_re <- tryCatch(plm(f_final, data = pdata, model = "random"), error = function(e) NULL)
        mod_pool <- tryCatch(plm(f_final, data = pdata, model = "pooling"), error = function(e) NULL)
        extract_m <- function(mod, type, ref1=NULL, ref2=NULL) {
          if(is.null(mod)) return(NULL)
          info <- get_plm_info(mod)
          fp <- NA; hp <- NA
          if(type == "Fixed Effects" && !is.null(ref1)) { t <- tryCatch(pFtest(mod, ref1), error=function(e) NULL); if(!is.null(t)) fp <- t$p.value }
          if(type == "Random Effects" && !is.null(ref2)) { t <- tryCatch(phtest(ref2, mod), error=function(e) NULL); if(!is.null(t)) hp <- t$p.value }
          data.frame(Dir = dir, Modelo = type, Vars = length(vars_selected), AIC = round(info["AIC"], 2), BIC = round(info["BIC"], 2), Adj_R2 = round(summary(mod)$r.squared["adjrsq"], 4), P_F_Test = ifelse(is.na(fp), "-", format.pval(fp, digits=3)), P_Hausman = ifelse(is.na(hp), "-", format.pval(hp, digits=3)), Formula = paste(vars_selected, collapse=" + "))
        }
        results_list[[counter]] <- extract_m(mod_fe, "Fixed Effects", ref1=mod_pool); counter <- counter + 1
        results_list[[counter]] <- extract_m(mod_re, "Random Effects", ref2=mod_fe); counter <- counter + 1
      }
      final_df <- do.call(rbind, results_list)
      if(!is.null(final_df)) {
        final_df <- final_df[order(final_df$AIC), ]
        comparison_results(final_df)
        best_row <- final_df[1, ]
        best_f <- as.formula(paste(input$comp_dep, "~", best_row$Formula))
        best_t <- ifelse(best_row$Modelo == "Fixed Effects", "within", "random")
        best_model_obj(plm(best_f, data = pdata, model = best_t))
      }
    })
  })
  
  output$comparison_table <- DT::renderDataTable({ req(comparison_results()); datatable(comparison_results(), selection = "single", options = list(pageLength = 5)) }, server = FALSE)
  output$best_model_summary <- renderPrint({ req(best_model_obj()); summary(best_model_obj()) })
  
  vif_results <- eventReactive(input$btn_calc_vif, {
    req(input$vif_idvs, data_filtered())
    if(length(input$vif_idvs) < 2) { showNotification("Selecione ao menos 2 variáveis.", type="error"); return(NULL) }
    df_vif <- data_filtered() %>% dplyr::select(all_of(input$vif_idvs))
    df_vif[sapply(df_vif, is.infinite)] <- NA
    df_vif <- na.omit(df_vif)
    df_vif <- df_vif[, sapply(df_vif, function(x) var(x) > 0)]
    if(ncol(df_vif) < 2) { showNotification("Menos de 2 variáveis com variância > 0.", type="error"); return(NULL) }
    if(nrow(df_vif) < ncol(df_vif) + 2) { showNotification("Dados insuficientes para VIF.", type="error"); return(NULL) }
    tryCatch({
      form <- as.formula(paste("rnorm(nrow(df_vif)) ~", paste(names(df_vif), collapse = "+")))
      mod <- lm(form, data = df_vif)
      aliases <- alias(mod)$Complete
      if(!is.null(aliases)) { showNotification(paste("Colinearidade perfeita detectada.", paste(rownames(aliases), collapse=", ")), type="error"); return(NULL) }
      vif_vals <- car::vif(mod)
      if(is.matrix(vif_vals)) vif_vals <- vif_vals[,1]
      data.frame(Variavel = names(vif_vals), VIF = as.numeric(vif_vals)) %>% arrange(desc(VIF))
    }, error = function(e) { showNotification(paste("Erro VIF:", e$message), type="error"); NULL })
  })
  output$vif_result_table <- renderTable({ req(vif_results()); vif_results() })
  output$vif_plot <- renderPlot({
    req(vif_results())
    df <- vif_results()
    ggplot(df, aes(x = reorder(Variavel, VIF), y = VIF, fill = VIF > 10)) + geom_col() + geom_hline(yintercept = 10, linetype="dashed", color="red") + coord_flip() + scale_fill_manual(values = c("TRUE"="#e74c3c", "FALSE"="#18bc9c"), name = "Crítico (>10)") + theme_minimal() + labs(title = "VIF por Variável", x = NULL, y = "VIF")
  })
}

shinyApp(ui, server)

