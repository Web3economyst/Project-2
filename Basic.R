# ==========================================================
# 📊 ANÁLISE SNIS COMPLETA — Regressões + Comparação AIC/BIC
# ==========================================================

# ---- 1) Pacotes ----
library(tidyverse)
library(plm)
library(car)
library(MASS)
library(purrr)
library(dplyr)
library(ggplot2)
library(tibble)
library(viridis)


# ---- 2) Dados ----
dados <- read.csv("sul_snis_municipio_agua_esgoto.csv", stringsAsFactors = FALSE)

# ---- 3) LIMPEZA + HEATMAP DE MISSING VALUES ----
dados <- dados %>%
  dplyr::mutate(across(where(is.character), ~na_if(.x, ""))) %>%
  dplyr::mutate(across(where(is.numeric), ~ifelse(is.infinite(.x), NA, .x))) %>%
  dplyr::filter(!is.na(id_municipio), !is.na(ano))

# Heatmap de Missing Values por UF e Ano
if (all(c("sigla_uf", "ano") %in% names(dados))) {
  excluidas <- c("id_municipio", "ano", "sigla_uf")
  num_cols <- setdiff(names(dados)[sapply(dados, is.numeric)], excluidas)
  
  missing_por_ano_uf <- dados %>%
    dplyr::group_by(sigla_uf, ano) %>%
    dplyr::summarise(
      total_missing = sum(is.na(across(all_of(num_cols)))),
      total_campos = n() * length(num_cols),
      .groups = "drop"
    ) %>%
    dplyr::mutate(perc_missing = ifelse(total_campos > 0, 100 * total_missing / total_campos, 0))
  
  if (nrow(missing_por_ano_uf) > 0) {
    ggplot(missing_por_ano_uf, aes(x = factor(ano), y = sigla_uf, fill = perc_missing)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.1f", perc_missing)), color = "black", size = 3) +
      scale_fill_viridis(option = "plasma", name = "% Missing") +
      labs(
        title = "Heatmap de Missing Values por Estado e Ano",
        x = "Ano", y = "Estado"
      ) +
      theme_minimal(base_size = 12)
  }
}

# ---- Remover variáveis com mais de 30% de missing (mantendo dependentes) ----
limite_missing <- 0.3
variaveis_dependentes <- c("populacao_atendida_agua", "populacao_atendida_esgoto")

pct_missing <- colMeans(is.na(dados))
variaveis_excluir <- names(pct_missing[pct_missing > limite_missing & !(names(pct_missing) %in% variaveis_dependentes)])

if (length(variaveis_excluir) > 0) {
  cat("🔸 Removendo variáveis com mais de 30% de missing (exceto dependentes):\n")
  print(variaveis_excluir)
  dados <- dados %>% dplyr::select(-all_of(variaveis_excluir))
}

# ---- 4) Estatísticas descritivas ----
num_vars <- dados[, sapply(dados, is.numeric)]
num_vars <- num_vars[, !(names(num_vars) %in% c("id_municipio", "ano", "sigla_uf")), drop = FALSE]

if (ncol(num_vars) > 0) {
  desc_stats <- as.data.frame(t(sapply(num_vars, function(x) {
    c(
      mean   = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd     = sd(x, na.rm = TRUE),
      min    = min(x, na.rm = TRUE),
      max    = max(x, na.rm = TRUE)
    )
  })))
  print(head(desc_stats, 10))
} else {
  message("Nenhuma variável numérica encontrada.")
}

# ---- 5) CORRELAÇÕES ----
# Usar apenas variáveis restantes (numéricas)
dados_corr <- dados[, sapply(dados, is.numeric)]
cols_excluir <- c("id_municipio", "ano", "sigla_uf")
dados_corr <- dados_corr[, !(names(dados_corr) %in% cols_excluir), drop = FALSE]

# Garantir que dependentes estão presentes
vars_interesse <- c("populacao_atendida_agua", "populacao_atendida_esgoto")
vars_faltantes <- setdiff(vars_interesse, names(dados_corr))
if (length(vars_faltantes) > 0) {
  warning(paste("⚠️ Variáveis dependentes ausentes:", paste(vars_faltantes, collapse = ", ")))
}

# Calcular matriz de correlação
cor_mat <- cor(dados_corr, use = "pairwise.complete.obs")

# Ordenar variáveis por similaridade
cor_dist <- as.dist(1 - abs(cor_mat))
hc <- hclust(cor_dist)
cor_mat <- cor_mat[hc$order, hc$order]

# Converter p/ formato longo
cor_melt <- as.data.frame(as.table(cor_mat))
names(cor_melt) <- c("Var1", "Var2", "value")

# ---- 5A) Heatmap geral ----
ggplot(cor_melt, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0, name = "Correlação"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Mapa de Correlações (Pearson) — Variáveis com até 30% de Missing",
    x = "", y = ""
  )

# ---- 5B) Top 20 correlações separadas por variável dependente ----

plot_top_corr <- function(data, var_ref, top_n = 20) {
  # Remover a outra dependente manualmente
  outras_dependentes <- setdiff(c("populacao_atendida_agua", "populacao_atendida_esgoto"), var_ref)
  data_filtrada <- data[, !(names(data) %in% outras_dependentes), drop = FALSE]
  
  # Calcular correlações
  corrs <- sapply(data_filtrada, function(x) cor(x, data_filtrada[[var_ref]], use = "pairwise.complete.obs"))
  
  # Montar data frame ordenado
  corr_df <- tibble(
    variavel = names(corrs),
    correlacao = as.numeric(corrs)
  ) %>%
    filter(!is.na(correlacao), variavel != var_ref) %>%
    arrange(desc(abs(correlacao))) %>%
    slice_head(n = top_n)
  
  # Gerar gráfico
  ggplot(corr_df, aes(x = reorder(variavel, correlacao), y = correlacao, fill = correlacao)) +
    geom_col() +
    coord_flip() +
    scale_fill_gradient2(low = "#d62728", mid = "white", high = "#1f77b4", midpoint = 0) +
    labs(
      title = paste0("Top ", top_n, " variáveis mais correlacionadas com ", var_ref),
      x = "Variável",
      y = "Correlação de Pearson"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 10)
    )
}

# ---- Gerar gráficos individuais ----
if ("populacao_atendida_agua" %in% names(dados_corr)) {
  print(plot_top_corr(dados_corr, "populacao_atendida_agua", top_n = 20))
}

if ("populacao_atendida_esgoto" %in% names(dados_corr)) {
  print(plot_top_corr(dados_corr, "populacao_atendida_esgoto", top_n = 20))
}

# ---- 6) Transformações log ----
cat("🔹 Aplicando transformação log (log1p) nas variáveis numéricas...\n")

df_log <- dados

# Evita log de valores negativos
num_cols <- names(df_log)[sapply(df_log, is.numeric)]
for (col in num_cols) {
  if (any(df_log[[col]] < 0, na.rm = TRUE)) {
    warning(paste("⚠️ Variável", col, "contém valores negativos — será ignorada no log."))
  } else {
    df_log[[col]] <- log1p(df_log[[col]])
  }
}

# ---- 7) Regressões Pooled e OLS (versão robusta) ----
if (!("id_municipio" %in% names(df_log)) | !("ano" %in% names(df_log))) {
  stop("❌ As colunas 'id_municipio' e 'ano' são necessárias para a análise de painel.")
}

cat("🔹 Criando estrutura de painel...\n")
df_painel <- df_log %>%
  filter(!is.na(id_municipio), !is.na(ano))

df_painel <- pdata.frame(df_painel, index = c("id_municipio", "ano"))

# Dependentes existentes
dependentes <- c("populacao_atendida_agua", "populacao_atendida_esgoto")
dependentes <- dependentes[dependentes %in% names(df_painel)]

if (length(dependentes) == 0) stop("Nenhuma variável dependente encontrada.")

# ---- Função auxiliar ----
rodar_modelos <- function(dep, data) {
  cat("\n==========================\n")
  cat("📘 MODELOS PARA:", dep, "\n")
  cat("==========================\n")
  
  independentes <- setdiff(names(data), c(dep, "id_municipio", "ano"))
  independentes <- independentes[sapply(data[, independentes, drop = FALSE], function(x) !all(is.na(x)))]
  
  # Checa colinearidade
  test_formula <- as.formula(paste(dep, "~", paste(independentes, collapse = " + ")))
  alias_check <- alias(lm(test_formula, data = as.data.frame(data)))
  if (length(alias_check$Complete) > 0) {
    cat("⚠️ Variáveis colineares detectadas e removidas:\n")
    print(names(alias_check$Complete))
    independentes <- setdiff(independentes, names(alias_check$Complete))
  }
  
  formula <- as.formula(paste(dep, "~", paste(independentes, collapse = " + ")))
  
  modelos <- list()
  
  # --- Modelo Pooled ---
  cat("➡️ Rodando modelo Pooled OLS...\n")
  modelos$pool <- tryCatch({
    plm(formula, data = data, model = "pooling")
  }, error = function(e) {
    cat("❌ Erro no modelo Pooled:", e$message, "\n")
    NULL
  })
  
  # --- Modelo OLS ---
  cat("➡️ Rodando modelo OLS...\n")
  modelos$ols <- tryCatch({
    lm(formula, data = as.data.frame(data))
  }, error = function(e) {
    cat("❌ Erro no modelo OLS:", e$message, "\n")
    NULL
  })
  
  # --- Stepwise ---
  modelos$step <- tryCatch({
    if (!is.null(modelos$ols)) {
      cat("🔎 Seleção Stepwise AIC...\n")
      stepAIC(modelos$ols, direction = "both", trace = FALSE)
    } else NULL
  }, error = function(e) {
    cat("❌ Stepwise falhou:", e$message, "\n")
    NULL
  })
  
  # --- Métricas AIC/BIC ---
  modelos$metrics <- tibble(
    Modelo = c("OLS", "Pooled")[c(!is.null(modelos$ols), !is.null(modelos$pool))],
    AIC = c(if (!is.null(modelos$ols)) AIC(modelos$ols) else NA,
            if (!is.null(modelos$pool)) AIC(modelos$pool) else NA),
    BIC = c(if (!is.null(modelos$ols)) BIC(modelos$ols) else NA,
            if (!is.null(modelos$pool)) BIC(modelos$pool) else NA),
    Dependente = dep
  )
  
  return(modelos)
}

# ---- 8) Rodar para cada dependente ----
resultados <- lapply(dependentes, function(dep) rodar_modelos(dep, df_painel))

# ---- 9) Comparação final dos modelos ----
comparacao <- bind_rows(lapply(resultados, function(x) x$metrics))

cat("\n\n📊 COMPARAÇÃO FINAL DE MODELOS (AIC/BIC):\n")
print(comparacao)

# ---- 10) Visualização Comparativa ----
if (nrow(comparacao) > 0) {
  ggplot(comparacao, aes(x = Dependente, y = AIC, fill = Modelo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = round(AIC, 1)), position = position_dodge(0.9), vjust = -0.3, size = 3) +
    theme_minimal() +
    labs(title = "Comparação de AIC entre Modelos", y = "AIC", x = "Variável Dependente")
  
  ggplot(comparacao, aes(x = Dependente, y = BIC, fill = Modelo)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = round(BIC, 1)), position = position_dodge(0.9), vjust = -0.3, size = 3) +
    theme_minimal() +
    labs(title = "Comparação de BIC entre Modelos", y = "BIC", x = "Variável Dependente")
} else {
  cat("⚠️ Nenhum modelo válido para comparar.\n")
}

