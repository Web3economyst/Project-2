# ==========================================================
# 📊 ANÁLISE SNIS — Regressão + Heatmaps Corrigidos
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

# ---- Dependente e independentes (com códigos SNIS) ----
dependente <- "populacao_atendida_agua"

variaveis_independentes <- c(
  "receita_operacional_direta_agua",         
  "receita_operacional_direta_esgoto",
  "receita_operacional_direta_agua_exportada",
  "receita_operacional_direta_esgoto_importado",
  "receita_operacional_indireta",
  "arrecadacao_total",
  "credito_areceber",
  "despesa_pessoal",
  "despesa_produto_quimico",
  "despesa_energia",
  "despesa_servico_terceiro",
  "despesa_exploracao",
  "despesas_juros_divida",
  "despesa_total_servico",
  "despesa_agua_importada",
  "despesa_fiscal",
  "despesa_fiscal_nao_computada",
  "despesa_amortizacao_divida",
  "despesa_esgoto_exportado",
  "receita_operacional",
  "investimento_total_prestador",
  "investimento_total_municipio",
  "investimento_total_estado",
  "volume_agua_produzido",
  "consumo_eletrico_sistemas_agua"
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

# Calcular % de missing por variável e ano
heatmap_indep_ano_var <- dados %>%
  group_by(ano) %>%
  summarise(across(all_of(variaveis_existentes),
                   ~100 * mean(is.na(.x)),  # % missing por variável
                   .names = "{.col}")) %>%
  pivot_longer(
    cols = all_of(variaveis_existentes),
    names_to = "variavel",
    values_to = "perc_missing"
  )

# Plotar heatmap com variáveis no eixo Y e anos no X
ggplot(heatmap_indep_ano_var, aes(x = factor(ano), y = variavel, fill = perc_missing)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", perc_missing)),
            size = 3, color = "black") +
  scale_fill_viridis(option = "plasma", name = "% Missing", direction = -1) +
  labs(
    title = "📊 Heatmap 2 — % Missing das Variáveis Independentes por Ano",
    x = "Ano", y = "Variável Independente"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ==========================================================
# 🔹 CORRELAÇÕES: apenas DEPENDENTE vs CADA INDEPENDENTE
# ==========================================================

# Dependente e independentes (certifica-se que existem no df)
dependente <- "populacao_atendida_agua"
vars_corr <- intersect(c(dependente, variaveis_existentes), names(dados))
indep_vars_present <- setdiff(vars_corr, dependente)

if (length(indep_vars_present) == 0) {
  stop("Nenhuma variável independente presente para correlacionar com a dependente.")
}

# Calcular correlações pairwise (dependente vs cada independente)
corr_list <- sapply(indep_vars_present, function(v) {
  x <- dados[[dependente]]
  y <- dados[[v]]
  # usa pairwise.complete.obs para ignorar pares com NA
  cor_val <- tryCatch(cor(x, y, use = "pairwise.complete.obs"), error = function(e) NA)
  return(cor_val)
}, simplify = TRUE, USE.NAMES = TRUE)

corr_df <- tibble(
  variavel = names(corr_list),
  correlacao = as.numeric(corr_list)
) %>%
  mutate(abs_cor = abs(correlacao)) %>%
  arrange(desc(abs_cor))

# Mostrar tabela ordenada
cat("\n🔹 Correlações (populacao_atendida_agua vs cada independente) — ordenadas por |r|:\n")
print(corr_df)

# Plot: barras horizontais ordenadas por |correlação|
library(ggplot2)
ggplot(corr_df, aes(x = reorder(variavel, correlacao), y = correlacao, fill = correlacao)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(correlacao, 3)), hjust = ifelse(corr_df$correlacao >= 0, -0.1, 1.1), size = 3) +
  scale_fill_gradient2(low = "#d62728", mid = "white", high = "#1f77b4", midpoint = 0) +
  labs(
    title = "Correlação: populacao_atendida_agua vs Variáveis Independentes",
    x = "Variável Independente",
    y = "Correlação de Pearson (pairwise)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 8), legend.position = "none")

# Opcional: mostrar apenas top N
top_n_show <- 20
cat("\n🔹 Top", top_n_show, "variáveis por |correlação|:\n")
print(head(corr_df, top_n_show))


# ==========================================================
# 🔹 TESTE DE COLINEARIDADE (VIF)
# ==========================================================
library(car)

# ---- 1) Preparação do dataframe ----
# (usa apenas dependente e independentes, ignora NAs)
df_vif <- dados[, c(dependente, variaveis_existentes)]
df_vif <- df_vif[complete.cases(df_vif), ]

# ---- 2) Transformação log1p nas numéricas ----
num_cols_vif <- names(df_vif)[sapply(df_vif, is.numeric)]
num_cols_vif <- setdiff(num_cols_vif, dependente)

for (col in num_cols_vif) {
  if (all(df_vif[[col]] >= 0, na.rm = TRUE)) {
    df_vif[[col]] <- log1p(df_vif[[col]])
  }
}

cat("✅ Dados preparados para o teste de colinearidade.\n")

# ---- 3) Rodar regressão OLS para calcular VIF ----
formula_vif <- as.formula(paste(dependente, "~", paste(variaveis_existentes, collapse = " + ")))
modelo_vif <- tryCatch(lm(formula_vif, data = df_vif), error = function(e) NULL)

if (!is.null(modelo_vif)) {
  
  # ---- 4) Calcular VIF ----
  vif_vals <- vif(modelo_vif)
  vif_df <- tibble(variavel = names(vif_vals), VIF = as.numeric(vif_vals)) %>%
    arrange(desc(VIF))
  
  cat("\n📊 Top variáveis com maior VIF (multicolinearidade potencial):\n")
  print(head(vif_df, 15))
  
  # ---- 5) Calcular Número de Condição ----
  X <- model.matrix(formula_vif, data = df_vif)[, -1, drop = FALSE]  # remove intercepto
  cond_number <- tryCatch(kappa(X, exact = TRUE), error = function(e) NA)
  
  cat("\n### Número de Condição ###\n")
  print(cond_number)
  
  if (!is.na(cond_number)) {
    if (cond_number < 30) {
      cat("🟢 Baixa multicolinearidade\n")
    } else if (cond_number < 100) {
      cat("🟡 Moderada multicolinearidade\n")
    } else {
      cat("🔴 Alta multicolinearidade (atenção)\n")
    }
  }
  
  # ---- 6) Plotar gráfico de VIF ----
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
  cat("⚠️ Falha no cálculo de VIF: modelo OLS não pôde ser ajustado.\n")
}

# ==========================================================
# 🔹 VARIÁVEL DE LUCRO OPERACIONAL + TESTE DE MULTICOLINEARIDADE FINAL
# ==========================================================

cat("\n💰 Criando variável de lucro operacional líquido (lucro_op)...\n")

# ---- 1) Criar variável lucro_op ----
dados <- dados %>%
  mutate(lucro_op = receita_operacional_direta_agua - despesa_exploracao)

# ---- 2) Atualizar lista de variáveis independentes ----
variaveis_independentes <- setdiff(variaveis_independentes, c(
  "receita_operacional_direta_agua",
  "despesa_exploracao",
  "receita_operacional", 
  "despesa_total_servico",
  "arrecadacao_total"
))
variaveis_independentes <- c(variaveis_independentes, "lucro_op")

cat("\nVariáveis independentes atualizadas:\n")
print(variaveis_independentes)

# ---- 3) Recalcular o VIF e Número de Condição ----
df_vif3 <- dados[, c(dependente, variaveis_independentes)]
df_vif3 <- df_vif3[complete.cases(df_vif3), ]

# Log-transform nas numéricas (mantendo escala compatível)
num_cols_vif3 <- names(df_vif3)[sapply(df_vif3, is.numeric)]
num_cols_vif3 <- setdiff(num_cols_vif3, dependente)
for (col in num_cols_vif3) {
  if (all(df_vif3[[col]] >= 0, na.rm = TRUE)) {
    df_vif3[[col]] <- log1p(df_vif3[[col]])
  }
}

# Modelo OLS revisado
formula_vif3 <- as.formula(paste(dependente, "~", paste(variaveis_independentes, collapse = " + ")))
modelo_vif3 <- lm(formula_vif3, data = df_vif3)

# ---- 4) Calcular VIF ----
library(car)
vif_vals3 <- vif(modelo_vif3)
vif_df3 <- tibble(variavel = names(vif_vals3), VIF = as.numeric(vif_vals3)) %>%
  arrange(desc(VIF))

cat("\n📊 VIF após substituição por lucro_op:\n")
print(head(vif_df3, 15))

# ---- 5) Número de Condição ----
X3 <- model.matrix(formula_vif3, data = df_vif3)[, -1, drop = FALSE]
cond_number3 <- tryCatch(kappa(X3, exact = TRUE), error = function(e) NA)

cat("\n### Número de Condição (após criar lucro_op) ###\n")
print(cond_number3)

if (!is.na(cond_number3)) {
  if (cond_number3 < 30) {
    cat("🟢 Baixa multicolinearidade\n")
  } else if (cond_number3 < 100) {
    cat("🟡 Moderada multicolinearidade\n")
  } else {
    cat("🔴 Alta multicolinearidade — verificar ajustes adicionais\n")
  }
}

# ---- 6) Plot VIF Final ----
library(ggplot2)
library(viridis)

ggplot(vif_df3, aes(x = reorder(variavel, VIF), y = VIF, fill = VIF)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = sprintf("%.2f", VIF)), hjust = -0.2, size = 3) +
  scale_fill_viridis(option = "plasma", direction = -1) +
  theme_minimal(base_size = 11) +
  labs(
    title = "📊 Fatores de Inflação da Variância (VIF)",
    subtitle = paste("Número de Condição:", round(cond_number3, 2)),
    x = "Variável",
    y = "VIF"
  ) +
  theme(axis.text.y = element_text(size = 8))


# ==========================================================
# ==========================================================
# 🔹 REGRESSÕES — Modelos de Painel (Pooled, FE, RE)
# ==========================================================

library(lmtest)
library(sandwich)
library(tibble)
library(ggplot2)

# --- Preparar dados de painel (não destrói 'dados' original) ---
cols_model <- c("id_municipio", "ano", dependente, variaveis_independentes)
cols_model <- intersect(cols_model, names(dados))
df_model <- dados[, cols_model]

# garantir ids
df_model <- df_model %>% filter(!is.na(id_municipio), !is.na(ano))

# Aplicar log1p às variáveis numéricas (exceto id/ano/dependente)
num_cols <- names(df_model)[sapply(df_model, is.numeric)]
num_cols <- setdiff(num_cols, c("id_municipio", "ano", dependente))
for (col in num_cols) {
  df_model[[col]] <- ifelse(!is.na(df_model[[col]]) & df_model[[col]] >= 0, log1p(df_model[[col]]), df_model[[col]])
}

# criar versão com complete.cases para as variáveis do modelo (plm/lm precisam disso)
df_complete <- df_model %>% tidyr::drop_na(all_of(c(dependente, variaveis_independentes)))

cat("\nLinhas usadas nos modelos (complete cases):", nrow(df_complete), "\n")

# transformar em painel
painel <- pdata.frame(df_complete, index = c("id_municipio", "ano"))

# --- Fórmula ---
formula_full <- as.formula(paste(dependente, "~", paste(variaveis_independentes, collapse = " + ")))
print(formula_full)

# --- 1) Pooled OLS ---
cat("\n➡️ Ajustando modelo Pooled OLS (pooling)...\n")
modelo_pooled <- plm(formula_full, data = painel, model = "pooling")

# --- 2) Efeitos Fixos (Within) ---
cat("➡️ Ajustando modelo Efeitos Fixos (within)...\n")
modelo_fe <- plm(formula_full, data = painel, model = "within")

# --- 3) Efeitos Aleatórios ---
cat("➡️ Ajustando modelo Efeitos Aleatórios (random)...\n")
modelo_re <- plm(formula_full, data = painel, model = "random")

# --- 4) Testes: F (FE vs Pooled) e Hausman (FE vs RE) ---
cat("\n📊 Testes de especificação:\n")
teste_f <- tryCatch(pFtest(modelo_fe, modelo_pooled), error = function(e) e)
teste_hausman <- tryCatch(phtest(modelo_fe, modelo_re), error = function(e) e)

cat("\n--- F test (FE vs Pooled) ---\n")
print(teste_f)
cat("\n--- Hausman test (FE vs RE) ---\n")
print(teste_hausman)

# --- 5) AIC / BIC (plm não tem método direto; calculamos via RSS) ---
calc_aic_bic_plm <- function(plm_model) {
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

# --- 6) Escolha automática pelo Hausman/F-test (prioriza FE se significativo) ---
modelo_escolhido <- modelo_pooled
modelo_label <- "Pooled OLS"

if (!inherits(teste_f, "error") && teste_f$p.value < 0.05) {
  modelo_escolhido <- modelo_fe
  modelo_label <- "Efeitos Fixos"
} else if (!inherits(teste_hausman, "error") && teste_hausman$p.value >= 0.05) {
  modelo_escolhido <- modelo_re
  modelo_label <- "Efeitos Aleatórios"
} else {
  modelo_escolhido <- modelo_pooled
  modelo_label <- "Pooled OLS"
}

cat(paste0("\n✅ Modelo sugerido: ", modelo_label, "\n"))

# --- 7) Coeficientes e SE robustos (clustered por id_municipio) ---
# vcovHC com cluster = "group" é apropriado para painel
vcov_cluster <- tryCatch(vcovHC(modelo_escolhido, type = "HC1", cluster = "group"), error = function(e) NULL)
coeftest_robust <- if (!is.null(vcov_cluster)) {
  lmtest::coeftest(modelo_escolhido, vcov. = vcov_cluster)
} else {
  lmtest::coeftest(modelo_escolhido)
}

cat("\n--- Coeficientes (com SE robustos por cluster quando possível) ---\n")
print(coeftest_robust)

# --- 8) Top variáveis por p-value (do modelo escolhido) ---
summary_escolhido <- summary(modelo_escolhido)
coefs_df <- as.data.frame(coef(summary_escolhido))
coefs_df <- coefs_df %>%
  rownames_to_column("variavel") %>%
  rename(Estimate = 2, StdError = 3, t_value = 4) %>%
  mutate(p_value = 2 * pt(-abs(t_value), df = summary_escolhido$df[2])) %>%
  arrange(p_value)

cat("\nTop 20 variáveis (modelo:", modelo_label, ") por p-value:\n")
print(head(coefs_df, 20))

# --- 9) Gráfico comparativo AIC/BIC ---
comparacao_long <- comparacao %>% tidyr::pivot_longer(cols = c(AIC, BIC), names_to = "Metric", values_to = "Value")
ggplot(comparacao_long, aes(x = Modelo, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(Value, 1)), position = position_dodge(0.9), vjust = -0.3, size = 3) +
  theme_minimal() +
  labs(title = "Comparação AIC/BIC — Modelos de Painel", y = "Valor", x = "")

cat("\n🔚 Processamento das regressões concluído.\n")

