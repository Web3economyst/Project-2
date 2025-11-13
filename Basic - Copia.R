# ==========================================================
# 📊 ANÁLISE SNIS — Esgoto: Regressão + Heatmaps + Colinearidade
# ==========================================================

# ---- Pacotes ----
library(tidyverse)
library(plm)
library(MASS)
library(car)
library(viridis)

# ---- Dados ----
dados <- read.csv("sul_snis_municipio_agua_esgoto.csv", stringsAsFactors = FALSE)

# ---- Limpeza mínima ----
dados <- dados %>%
  mutate(across(where(is.character), ~na_if(.x, ""))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.x), NA, .x))) %>%
  filter(!is.na(id_municipio), !is.na(ano))

# ==========================================================
# 🔹 Dependente e Independentes (Sistema de Esgoto)
# ==========================================================
dependente <- "populacao_atendida_esgoto"  # AG002

variaveis_independentes <- c(
  "receita_operacional_direta_esgoto",   # FN003    
  "arrecadacao_total",                   # FN006
  "credito_areceber",                    # FN008
  "despesa_exploracao",                  # FN014
  "despesa_pessoal",                     # FN013
  "despesa_produto_quimico",             # FN015
  "despesa_energia",                     # FN016
  "despesa_servico_terceiro",            # FN017
  "investimento_total_prestador",        # FN052
  "investimento_total_municipio",        # FN056
  "investimento_total_estado",           # FN060
  "consumo_eletrico_sistemas_esgoto",    # FN041
)

variaveis_existentes <- intersect(variaveis_independentes, names(dados))
cat("Variáveis independentes encontradas:\n")
print(variaveis_existentes)

# ==========================================================
# 🔹 HEATMAP 1 — % Missing agregado por Estado e Ano
# ==========================================================
if (all(c("sigla_uf", "ano") %in% names(dados))) {
  heatmap_estado_ano <- dados %>%
    group_by(sigla_uf, ano) %>%
    summarise(
      total_missing = sum(is.na(across(where(is.numeric)))),
      total_campos = n() * sum(sapply(dados, is.numeric)),
      perc_missing = 100 * total_missing / total_campos,
      .groups = "drop"
    )
  
  ggplot(heatmap_estado_ano, aes(x = factor(ano), y = sigla_uf, fill = perc_missing)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", perc_missing)), size = 3, color = "black") +
    scale_fill_viridis(option = "plasma", name = "% Missing") +
    labs(title = "📊 Heatmap 1 — % Missing agregado por Estado e Ano",
         x = "Ano", y = "Estado") +
    theme_minimal(base_size = 12)
} else {
  cat("⚠️ Coluna 'sigla_uf' ausente — Heatmap por estado não gerado.\n")
}

# ==========================================================
# 🔹 HEATMAP 2 — % Missing de cada Variável Independente por Ano
# ==========================================================
heatmap_indep_ano_var <- dados %>%
  group_by(ano) %>%
  summarise(across(all_of(variaveis_existentes),
                   ~100 * mean(is.na(.x)),
                   .names = "{.col}")) %>%
  pivot_longer(cols = all_of(variaveis_existentes),
               names_to = "variavel",
               values_to = "perc_missing")

ggplot(heatmap_indep_ano_var, aes(x = factor(ano), y = variavel, fill = perc_missing)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", perc_missing)),
            size = 3, color = "black") +
  scale_fill_viridis(option = "plasma", name = "% Missing", direction = -1) +
  labs(title = "📊 Heatmap 2 — % Missing das Variáveis Independentes por Ano",
       x = "Ano", y = "Variável Independente") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))

# ==========================================================
# 🔹 CORRELAÇÕES — Dependente vs Independentes
# ==========================================================
vars_corr <- intersect(c(dependente, variaveis_existentes), names(dados))
indep_vars_present <- setdiff(vars_corr, dependente)

corr_list <- sapply(indep_vars_present, function(v) {
  x <- dados[[dependente]]
  y <- dados[[v]]
  cor_val <- tryCatch(cor(x, y, use = "pairwise.complete.obs"), error = function(e) NA)
  return(cor_val)
}, simplify = TRUE, USE.NAMES = TRUE)

corr_df <- tibble(
  variavel = names(corr_list),
  correlacao = as.numeric(corr_list)
) %>%
  mutate(abs_cor = abs(correlacao)) %>%
  arrange(desc(abs_cor))

cat("\n🔹 Correlações (populacao_atendida_esgoto vs cada independente):\n")
print(corr_df)

ggplot(corr_df, aes(x = reorder(variavel, correlacao), y = correlacao, fill = correlacao)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(correlacao, 3)), 
            hjust = ifelse(corr_df$correlacao >= 0, -0.1, 1.1), size = 3) +
  scale_fill_gradient2(low = "#d62728", mid = "white", high = "#1f77b4", midpoint = 0) +
  labs(title = "Correlação: população atendida (esgoto) vs Variáveis Independentes",
       x = "Variável Independente", y = "Correlação de Pearson") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 8), legend.position = "none")

# ==========================================================
# 🔹 TESTE DE COLINEARIDADE (VIF)
# ==========================================================
cat("\nCalculando Fatores de Inflação da Variância (VIF)...\n")

df_vif <- dados[, c(dependente, variaveis_existentes)]
df_vif <- df_vif[complete.cases(df_vif), ]

num_cols_vif <- names(df_vif)[sapply(df_vif, is.numeric)]
num_cols_vif <- setdiff(num_cols_vif, dependente)
for (col in num_cols_vif) {
  if (all(df_vif[[col]] >= 0, na.rm = TRUE)) {
    df_vif[[col]] <- log1p(df_vif[[col]])
  }
}

formula_vif <- as.formula(paste(dependente, "~", paste(variaveis_existentes, collapse = " + ")))
modelo_vif <- tryCatch(lm(formula_vif, data = df_vif), error = function(e) NULL)

if (!is.null(modelo_vif)) {
  vif_vals <- vif(modelo_vif)
  vif_df <- tibble(variavel = names(vif_vals), VIF = as.numeric(vif_vals)) %>%
    arrange(desc(VIF))
  print(head(vif_df, 15))
  
  # Número de Condição
  X <- model.matrix(formula_vif, data = df_vif)[, -1, drop = FALSE]
  cond_number <- tryCatch(kappa(X, exact = TRUE), error = function(e) NA)
  
  cat("\n### Número de Condição ###\n")
  print(cond_number)
  
  if (!is.na(cond_number)) {
    if (cond_number < 30) {
      cat("🟢 Baixa multicolinearidade\n")
    } else if (cond_number < 100) {
      cat("🟡 Moderada multicolinearidade\n")
    } else {
      cat("🔴 Alta multicolinearidade\n")
    }
  }
  
  ggplot(vif_df, aes(x = reorder(variavel, VIF), y = VIF, fill = VIF)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = sprintf("%.2f", VIF)), hjust = -0.2, size = 3) +
    scale_fill_viridis(option = "plasma", direction = -1) +
    theme_minimal(base_size = 11) +
    labs(title = "📊 Fatores de Inflação da Variância (VIF)",
         subtitle = paste("Número de Condição:", round(cond_number, 2)),
         x = "Variável", y = "VIF") +
    theme(axis.text.y = element_text(size = 8))
} else {
  cat("⚠️ Falha no cálculo de VIF (possível singularidade).\n")
}


# ==========================================================
# 🔹 REGRESSÕES — Modelos de Painel (Pooled, FE, RE)
# ==========================================================

library(lmtest)
library(sandwich)
library(tibble)
library(ggplot2)

# --- Preparar dados de painel ---
cols_model <- c("id_municipio", "ano", dependente, variaveis_existentes)
cols_model <- intersect(cols_model, names(dados))
df_model <- dados[, cols_model]

df_model <- df_model %>%
  filter(!is.na(id_municipio), !is.na(ano))

# Log1p nas numéricas (sem alterar id/ano/dependente)
num_cols <- names(df_model)[sapply(df_model, is.numeric)]
num_cols <- setdiff(num_cols, c("id_municipio", "ano", dependente))
for (col in num_cols) {
  if (all(df_model[[col]] >= 0, na.rm = TRUE)) {
    df_model[[col]] <- log1p(df_model[[col]])
  }
}

# Remover NAs
df_complete <- df_model %>% tidyr::drop_na(all_of(c(dependente, variaveis_existentes)))
cat("\nLinhas usadas nos modelos (complete cases):", nrow(df_complete), "\n")

painel <- pdata.frame(df_complete, index = c("id_municipio", "ano"))

# --- Fórmula ---
formula_full <- as.formula(paste(dependente, "~", paste(variaveis_existentes, collapse = " + ")))
cat("\n📄 Fórmula do modelo:\n")
print(formula_full)

# --- Ajuste dos modelos ---
cat("\n➡️ Ajustando modelo Pooled OLS...\n")
modelo_pooled <- tryCatch(plm(formula_full, data = painel, model = "pooling"), error = function(e) e)

cat("➡️ Ajustando modelo Efeitos Fixos...\n")
modelo_fe <- tryCatch(plm(formula_full, data = painel, model = "within"), error = function(e) e)

cat("➡️ Ajustando modelo Efeitos Aleatórios...\n")
modelo_re <- tryCatch(plm(formula_full, data = painel, model = "random"), error = function(e) e)

# --- Testes de especificação ---
cat("\n📊 Testes de especificação (F e Hausman):\n")

if (!inherits(modelo_fe, "error") && !inherits(modelo_pooled, "error")) {
  teste_f <- tryCatch(pFtest(modelo_fe, modelo_pooled), error = function(e) e)
  cat("\n--- F test (Efeitos Fixos vs Pooled OLS) ---\n")
  print(teste_f)
} else {
  cat("⚠️ Não foi possível calcular o teste F — modelo FE ou Pooled falhou.\n")
}

if (!inherits(modelo_fe, "error") && !inherits(modelo_re, "error")) {
  teste_hausman <- tryCatch(phtest(modelo_fe, modelo_re), error = function(e) e)
  cat("\n--- Hausman test (Efeitos Fixos vs Aleatórios) ---\n")
  print(teste_hausman)
} else {
  cat("⚠️ Não foi possível calcular o teste Hausman — modelo FE ou RE falhou.\n")
  teste_hausman <- NULL
}

# --- Cálculo AIC/BIC (apenas se os modelos existirem) ---
calc_aic_bic_plm <- function(plm_model) {
  if (inherits(plm_model, "error")) return(list(AIC = NA, BIC = NA))
  res <- residuals(plm_model)
  n <- length(res)
  k <- length(coef(plm_model))
  rss <- sum(res^2, na.rm = TRUE)
  aic <- n * log(rss / n) + 2 * k
  bic <- n * log(rss / n) + log(n) * k
  list(AIC = aic, BIC = bic)
}

metrics_pooled <- calc_aic_bic_plm(modelo_pooled)
metrics_fe     <- calc_aic_bic_plm(modelo_fe)
metrics_re     <- calc_aic_bic_plm(modelo_re)

comparacao <- tibble(
  Modelo = c("Pooled OLS", "Efeitos Fixos", "Efeitos Aleatórios"),
  AIC = c(metrics_pooled$AIC, metrics_fe$AIC, metrics_re$AIC),
  BIC = c(metrics_pooled$BIC, metrics_fe$BIC, metrics_re$BIC)
)
cat("\n📈 Comparação AIC/BIC:\n")
print(comparacao)

# --- Escolha automática segura ---
modelo_escolhido <- modelo_pooled
modelo_label <- "Pooled OLS"

if (exists("teste_f") && !inherits(teste_f, "error") && teste_f$p.value < 0.05) {
  modelo_escolhido <- modelo_fe
  modelo_label <- "Efeitos Fixos"
} else if (!is.null(teste_hausman) && !inherits(teste_hausman, "error") && teste_hausman$p.value >= 0.05) {
  modelo_escolhido <- modelo_re
  modelo_label <- "Efeitos Aleatórios"
}

cat(paste0("\n✅ Modelo sugerido: ", modelo_label, "\n"))

# --- Coeficientes robustos ---
if (!inherits(modelo_escolhido, "error")) {
  vcov_cluster <- tryCatch(vcovHC(modelo_escolhido, type = "HC1", cluster = "group"), error = function(e) NULL)
  coeftest_robust <- if (!is.null(vcov_cluster)) {
    lmtest::coeftest(modelo_escolhido, vcov. = vcov_cluster)
  } else {
    lmtest::coeftest(modelo_escolhido)
  }
  
  cat("\n--- Coeficientes (erro-padrão robusto) ---\n")
  print(coeftest_robust)
}

cat("\n🔚 Regressões de painel concluídas com segurança.\n")


