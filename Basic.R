# ==========================================================
# 📊 ANÁLISE SNIS — Script final com seleção (FE/RE) usando AIC/BIC
# ==========================================================
# Cole e rode inteiro. Produz:
# - heatmaps, correlações, VIF, lucro_op
# - seleção de variáveis (forward / backward / stepwise)
#   para FE (within) e RE (random) usando AIC e BIC
# - modelos finais, ICs e justificativa automática da escolha
# ==========================================================

# ---- Pacotes ----
library(tidyverse)
library(plm)
library(MASS)
library(car)
library(viridis)
library(lmtest)
library(sandwich)
library(tibble)
library(ggplot2)
library(tidyr)

# ---- Dados ----
dados <- read.csv("sul_snis_municipio_agua_esgoto.csv", stringsAsFactors = FALSE)

# ---- Dependente e independentes (conforme você informou) ----
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

# manter apenas as existentes no dataset
variaveis_independentes <- intersect(variaveis_independentes, names(dados))
cat("Variáveis independentes consideradas:\n"); print(variaveis_independentes)

# ==========================================================
# 🔹 HEATMAP 1 — % Missing agregado por Estado e Ano (CORRIGIDO)
# ==========================================================
if (all(c("sigla_uf", "ano") %in% names(dados))) {
  
  heatmap_estado_ano <- dados %>%
    group_by(sigla_uf, ano) %>%
    summarise(
      # pega apenas colunas numéricas do grupo atual
      num_cols = sum(sapply(cur_data_all(), is.numeric)),
      
      # conta missing em todas as colunas numéricas
      total_missing = sum(sapply(cur_data_all()[, sapply(cur_data_all(), is.numeric), drop = FALSE],
                                 function(x) sum(is.na(x)))),
      
      # total de campos numéricos avaliados
      total_campos = n() * num_cols,
      
      # percentual
      perc_missing = 100 * total_missing / total_campos,
      .groups = "drop"
    )
  
  # plot
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
if (length(variaveis_independentes) > 0) {
  heatmap_indep_ano_var <- dados %>%
    group_by(ano) %>%
    summarise(across(all_of(variaveis_independentes),
                     ~100 * mean(is.na(.x)),
                     .names = "{.col}")) %>%
    pivot_longer(
      cols = all_of(variaveis_independentes),
      names_to = "variavel",
      values_to = "perc_missing"
    )
  
  ggplot(heatmap_indep_ano_var, aes(x = factor(ano), y = variavel, fill = perc_missing)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f%%", perc_missing)), size = 3, color = "black") +
    scale_fill_viridis(option = "plasma", name = "% Missing", direction = -1) +
    labs(title = "📊 Heatmap 2 — % Missing das Variáveis Independentes por Ano",
         x = "Ano", y = "Variável Independente") +
    theme_minimal(base_size = 11) +
    theme(axis.text.y = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1))
} else {
  cat("⚠️ Nenhuma variável independente encontrada para heatmap 2.\n")
}

# ==========================================================
# 🔹 CORRELAÇÕES: dependente vs cada independente (pairwise)
# ==========================================================
vars_corr <- intersect(c(dependente, variaveis_independentes), names(dados))
indep_vars_present <- setdiff(vars_corr, dependente)

if (length(indep_vars_present) == 0) {
  stop("Nenhuma variável independente presente para correlacionar com a dependente.")
}

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

cat("\n🔹 Correlações (populacao_atendida_agua vs cada independente) — ordenadas por |r|:\n")
print(corr_df)

ggplot(corr_df, aes(x = reorder(variavel, correlacao), y = correlacao, fill = correlacao)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(correlacao, 3)), hjust = ifelse(corr_df$correlacao >= 0, -0.1, 1.1), size = 3) +
  scale_fill_gradient2(low = "#d62728", mid = "white", high = "#1f77b4", midpoint = 0) +
  labs(title = "Correlação: populacao_atendida_agua vs Variáveis Independentes",
       x = "Variável Independente", y = "Correlação de Pearson (pairwise)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 8), legend.position = "none")

# ==========================================================
# 🔹 VIF inicial
# ==========================================================
df_vif <- dados[, c(dependente, variaveis_independentes)]
df_vif <- df_vif[complete.cases(df_vif), ]

if (nrow(df_vif) > 0 && length(variaveis_independentes) > 0) {
  num_cols_vif <- names(df_vif)[sapply(df_vif, is.numeric)]
  num_cols_vif <- setdiff(num_cols_vif, dependente)
  for (col in num_cols_vif) {
    if (all(df_vif[[col]] >= 0, na.rm = TRUE)) {
      df_vif[[col]] <- log1p(df_vif[[col]])
    }
  }
  
  formula_vif <- as.formula(paste(dependente, "~", paste(variaveis_independentes, collapse = " + ")))
  modelo_vif <- tryCatch(lm(formula_vif, data = df_vif), error = function(e) NULL)
  
  if (!is.null(modelo_vif)) {
    vif_vals <- tryCatch(vif(modelo_vif), error = function(e) NULL)
    if (!is.null(vif_vals)) {
      vif_df <- tibble(variavel = names(vif_vals), VIF = as.numeric(vif_vals)) %>% arrange(desc(VIF))
      cat("\n📊 Top variáveis com maior VIF (multicolinearidade potencial):\n")
      print(head(vif_df, 15))
      
      X <- model.matrix(formula_vif, data = df_vif)[, -1, drop = FALSE]
      cond_number <- tryCatch(kappa(X, exact = TRUE), error = function(e) NA)
      cat("\n### Número de Condição ###\n"); print(cond_number)
      if (!is.na(cond_number)) {
        if (cond_number < 30) cat("🟢 Baixa multicolinearidade\n") else
          if (cond_number < 100) cat("🟡 Moderada multicolinearidade\n") else cat("🔴 Alta multicolinearidade (atenção)\n")
      }
      
      ggplot(vif_df, aes(x = reorder(variavel, VIF), y = VIF, fill = VIF)) +
        geom_col() + coord_flip() +
        geom_text(aes(label = sprintf("%.2f", VIF)), hjust = -0.2, size = 3) +
        scale_fill_viridis(option = "plasma", direction = -1) +
        labs(title = "📊 Fatores de Inflação da Variância (VIF)",
             subtitle = paste("Número de Condição:", round(cond_number, 2)),
             x = "Variável", y = "VIF") +
        theme_minimal(base_size = 11) +
        theme(axis.text.y = element_text(size = 8))
    } else cat("⚠️ VIF não pôde ser calculado.\n")
  } else cat("⚠️ Modelo OLS para VIF não pôde ser ajustado.\n")
} else cat("⚠️ Dados insuficientes para VIF inicial.\n")

# ==========================================================
# 🔹 Criar lucro_op e atualizar variáveis
# ==========================================================
cat("\n💰 Criando variável lucro_op...\n")
dados <- dados %>% mutate(lucro_op = receita_operacional_direta_agua - despesa_exploracao)

variaveis_independentes <- setdiff(variaveis_independentes, c(
  "receita_operacional_direta_agua",
  "despesa_exploracao",
  "receita_operacional",
  "despesa_total_servico",
  "arrecadacao_total"
))
variaveis_independentes <- unique(c(variaveis_independentes, "lucro_op"))
cat("Variáveis independentes atualizadas:\n"); print(variaveis_independentes)

# Recalcular VIF (opcional)
df_vif3 <- dados[, c(dependente, variaveis_independentes)]
df_vif3 <- df_vif3[complete.cases(df_vif3), ]
if (nrow(df_vif3) > 0 && length(variaveis_independentes) > 0) {
  num_cols_vif3 <- names(df_vif3)[sapply(df_vif3, is.numeric)]
  num_cols_vif3 <- setdiff(num_cols_vif3, dependente)
  for (col in num_cols_vif3) {
    if (all(df_vif3[[col]] >= 0, na.rm = TRUE)) df_vif3[[col]] <- log1p(df_vif3[[col]])
  }
  formula_vif3 <- as.formula(paste(dependente, "~", paste(variaveis_independentes, collapse = " + ")))
  modelo_vif3 <- tryCatch(lm(formula_vif3, data = df_vif3), error = function(e) NULL)
  if (!is.null(modelo_vif3)) {
    vif_vals3 <- tryCatch(vif(modelo_vif3), error = function(e) NULL)
    if (!is.null(vif_vals3)) {
      vif_df3 <- tibble(variavel = names(vif_vals3), VIF = as.numeric(vif_vals3)) %>% arrange(desc(VIF))
      cat("\n📊 VIF após lucro_op:\n"); print(head(vif_df3, 15))
      X3 <- model.matrix(formula_vif3, data = df_vif3)[, -1, drop = FALSE]
      cond_number3 <- tryCatch(kappa(X3, exact = TRUE), error = function(e) NA)
      cat("\n### Número de Condição (após lucro_op) ###\n"); print(cond_number3)
    }
  }
}

# ==========================================================
# 🔥 Preparar painel para seleção/modelos
# ==========================================================
cols_model <- c("id_municipio", "ano", dependente, variaveis_independentes)
cols_model <- intersect(cols_model, names(dados))
df_model <- dados[, cols_model]
df_model <- df_model %>% filter(!is.na(id_municipio), !is.na(ano))

# log1p nas numéricas (exceto id/ano/dependente)
num_cols_model <- names(df_model)[sapply(df_model, is.numeric)]
num_cols_model <- setdiff(num_cols_model, c("id_municipio", "ano", dependente))
for (col in num_cols_model) {
  df_model[[col]] <- ifelse(!is.na(df_model[[col]]) & df_model[[col]] >= 0, log1p(df_model[[col]]), df_model[[col]])
}

df_complete <- df_model %>% tidyr::drop_na(all_of(c(dependente, variaveis_independentes)))
cat("\nLinhas usadas nos modelos (complete cases):", nrow(df_complete), "\n")
painel_base <- pdata.frame(df_complete, index = c("id_municipio", "ano"))

# ==========================================================
# 🔧 Funções utilitárias para IC (plm) — tenta logLik, fallback RSS
# ==========================================================
calc_ic_plm <- function(plm_model) {
  if (is.null(plm_model)) return(list(AIC = NA, BIC = NA, logLik = NA))
  ll <- tryCatch(as.numeric(logLik(plm_model)), error = function(e) NA)
  n <- tryCatch(length(residuals(plm_model)), error = function(e) NA)
  k <- tryCatch(length(coef(plm_model)), error = function(e) NA)
  if (!is.na(ll) && !is.na(k) && !is.na(n)) {
    aic <- -2 * ll + 2 * k
    bic <- -2 * ll + log(n) * k
    return(list(AIC = aic, BIC = bic, logLik = ll))
  }
  res <- tryCatch(residuals(plm_model), error = function(e) NA)
  if (is.na(res[1])) return(list(AIC = NA, BIC = NA, logLik = NA))
  rss <- sum(res^2, na.rm = TRUE)
  aic <- n * log(rss / n) + 2 * k
  bic <- n * log(rss / n) + log(n) * k
  return(list(AIC = aic, BIC = bic, logLik = NA))
}

# ==========================================================
# 🔁 Função genérica de seleção por IC (forward/backward/stepwise)
# ==========================================================
step_selection_ic <- function(vars,
                              model_type = c("within", "random"),
                              ic = c("AIC", "BIC"),
                              direction = c("forward", "backward", "both"),
                              panel_df) {
  model_type <- match.arg(model_type)
  ic <- match.arg(ic)
  direction <- match.arg(direction)
  fit_model <- function(varset) {
    rhs <- if (length(varset) == 0) "1" else paste(varset, collapse = " + ")
    f <- as.formula(paste(dependente, "~", rhs))
    tryCatch(plm(f, data = panel_df, model = model_type), error = function(e) NULL)
  }
  get_ic_val <- function(mod) {
    if (is.null(mod)) return(Inf)
    val <- calc_ic_plm(mod)
    if (ic == "AIC") return(val$AIC) else return(val$BIC)
  }
  # Forward
  if (direction == "forward") {
    selected <- c(); remaining <- vars; improved <- TRUE
    while (improved && length(remaining) > 0) {
      improved <- FALSE
      current_mod <- fit_model(selected); current_ic <- get_ic_val(current_mod)
      ic_add <- sapply(remaining, function(v) get_ic_val(fit_model(c(selected, v))))
      if (all(is.infinite(ic_add))) break
      best <- which.min(ic_add); if (ic_add[best] + 1e-8 < current_ic) {
        selected <- c(selected, names(ic_add)[best])
        remaining <- setdiff(remaining, names(ic_add)[best])
        improved <- TRUE
      }
    }
    return(selected)
  }
  # Backward
  if (direction == "backward") {
    selected <- vars; improved <- TRUE
    while (improved && length(selected) > 0) {
      improved <- FALSE
      current_mod <- fit_model(selected); current_ic <- get_ic_val(current_mod)
      if (length(selected) == 1) break
      ic_remove <- sapply(selected, function(v) get_ic_val(fit_model(setdiff(selected, v))))
      if (all(is.infinite(ic_remove))) break
      best <- which.min(ic_remove)
      if (ic_remove[best] + 1e-8 < current_ic) {
        selected <- setdiff(selected, names(ic_remove)[best])
        improved <- TRUE
      }
    }
    return(selected)
  }
  # Both (stepwise)
  if (direction == "both") {
    selected <- c(); remaining <- vars; improved_outer <- TRUE
    while (improved_outer) {
      improved_outer <- FALSE
      # forward step
      ic_add <- if (length(setdiff(vars, selected))==0) numeric(0) else sapply(setdiff(vars, selected), function(v) get_ic_val(fit_model(c(selected, v))))
      if (length(ic_add) > 0 && !all(is.infinite(ic_add))) {
        current_mod <- fit_model(selected); current_ic <- get_ic_val(current_mod)
        best_add <- which.min(ic_add)
        if (ic_add[best_add] + 1e-8 < current_ic) {
          selected <- c(selected, names(ic_add)[best_add])
          improved_outer <- TRUE
        }
      }
      # backward prune
      repeat {
        if (length(selected) <= 1) break
        current_mod2 <- fit_model(selected); current_ic2 <- get_ic_val(current_mod2)
        ic_remove <- sapply(selected, function(v) get_ic_val(fit_model(setdiff(selected, v))))
        if (all(is.infinite(ic_remove))) break
        best_remove <- which.min(ic_remove)
        if (ic_remove[best_remove] + 1e-8 < current_ic2) {
          selected <- setdiff(selected, names(ic_remove)[best_remove])
          improved_outer <- TRUE
        } else break
      }
    }
    return(selected)
  }
}

# ==========================================================
# 🔍 Executar seleção: FE & RE × AIC & BIC × directions (fwd/bwd/both)
# ==========================================================
candidates <- variaveis_independentes
if (length(candidates) == 0) stop("Nenhuma variável candidata disponível para seleção.")

directions <- c("forward", "backward", "both")
ics <- c("AIC", "BIC")
model_types <- c("within", "random")

selection_results <- list()

for (m in model_types) {
  for (ic_val in ics) {
    for (dir in directions) {
      key <- paste(m, dir, ic_val, sep = "_")
      cat("\nExecutando seleção:", key, "...\n")
      sel <- tryCatch(step_selection_ic(candidates, model_type = m, ic = ic_val, direction = dir, panel_df = painel_base),
                      error = function(e) { message("erro seleção: ", e$message); return(character(0)) })
      selection_results[[key]] <- sel
      cat("-> selecionadas (", key, "):", paste(sel, collapse = ", "), "\n")
    }
  }
}

# ==========================================================
# 🔧 Ajustar modelos finais para cada seleção e coletar ICs
# ==========================================================
fit_and_ic <- function(varset, model_type) {
  rhs <- if (length(varset) == 0) "1" else paste(varset, collapse = " + ")
  f <- as.formula(paste(dependente, "~", rhs))
  mod <- tryCatch(plm(f, data = painel_base, model = model_type), error = function(e) NULL)
  icvals <- calc_ic_plm(mod)
  # robust clustered SE
  vcov_clust <- tryCatch(vcovHC(mod, type = "HC1", cluster = "group"), error = function(e) NULL)
  coefs_rob <- if (!is.null(mod) && !is.null(vcov_clust)) tryCatch(lmtest::coeftest(mod, vcov. = vcov_clust), error = function(e) NULL) else NULL
  list(model = mod, vars = varset, AIC = icvals$AIC, BIC = icvals$BIC, coefs = coefs_rob, formula = if (!is.null(mod)) formula(mod) else f)
}

results <- list()
for (nm in names(selection_results)) {
  parts <- strsplit(nm, "_")[[1]]
  model_type <- parts[1]
  dir <- parts[2]
  ic_used <- parts[3]
  sel_vars <- selection_results[[nm]]
  res <- fit_and_ic(sel_vars, model_type)
  results[[nm]] <- res
}

# ==========================================================
# 🔎 Resumo final organizado: cada linha FE/RE × direção × IC
# (versão robusta — não usa dplyr::select para evitar conflitos)
# ==========================================================
resumo <- tibble(
  key        = names(results),
  model_type = sapply(names(results), function(n) strsplit(n, "_")[[1]][1]),
  direction  = sapply(names(results), function(n) strsplit(n, "_")[[1]][2]),
  ic_used    = sapply(names(results), function(n) strsplit(n, "_")[[1]][3]),
  n_vars     = sapply(results, function(x) length(x$vars)),
  vars       = sapply(results, function(x) paste(x$vars, collapse = ", ")),
  AIC        = as.numeric(sapply(results, function(x) ifelse(is.null(x$AIC) || is.nan(x$AIC), NA, x$AIC))),
  BIC        = as.numeric(sapply(results, function(x) ifelse(is.null(x$BIC) || is.nan(x$BIC), NA, x$BIC)))
)

# criar coluna com AIC e BIC lado a lado (formatada)
resumo$AIC_BIC <- paste0(
  "AIC=", ifelse(is.na(resumo$AIC), "NA", sprintf("%.2f", resumo$AIC)),
  " | BIC=", ifelse(is.na(resumo$BIC), "NA", sprintf("%.2f", resumo$BIC))
)

cat("\n=== Resumo geral das seleções e ICs (AIC e BIC lado a lado) ===\n")

# imprimir apenas as colunas desejadas usando indexação base (seguro contra masks)
cols_print <- c("key", "model_type", "direction", "ic_used", "n_vars", "vars", "AIC_BIC")
print(as.data.frame(resumo)[ , cols_print, drop = FALSE])


# ==========================================================
# 🔍 Identificar melhores modelos por AIC e por BIC
# ==========================================================
best_aic_row <- resumo %>% filter(!is.na(AIC)) %>% arrange(AIC) %>% slice(1)
best_bic_row <- resumo %>% filter(!is.na(BIC)) %>% arrange(BIC) %>% slice(1)

cat("\nMelhor por AIC:\n"); print(best_aic_row)
cat("\nMelhor por BIC:\n"); print(best_bic_row)

# carregar objeto do melhor por BIC (prioriza parcimônia)
best_bic_key <- best_bic_row$key[1]
modelo_final_info <- results[[best_bic_key]]

cat("\n\n===== MODELO FINAL SUGERIDO (por BIC) =====\n")
cat("Chave:", best_bic_key, "\n")
cat("Estimador (within=random):", best_bic_row$model_type[1], "\n")
cat("Método de seleção:", best_bic_row$direction[1], "com critério", best_bic_row$ic_used[1], "\n")
cat("Variáveis selecionadas:", best_bic_row$vars[1], "\n")
cat("Número de variáveis:", best_bic_row$n_vars[1], "\n")
cat("BIC:", best_bic_row$BIC[1], " | AIC:", best_bic_row$AIC[1], "\n\n")

# Justificativa automatizada (texto)
justificativa <- paste0(
  "Justificativa: este modelo foi escolhido porque apresentou o menor BIC entre todas as combinações testadas\n",
  "(tipos de estimador: FE (within) e RE (random); métodos de seleção: forward/backward/stepwise; critérios: AIC e BIC).\n",
  "BIC penaliza mais a complexidade que AIC, então selecionamos o modelo mais parcimonioso que ainda explica adequadamente a variabilidade.\n",
  "Se preferir priorizar ajuste preditivo, escolha o modelo com menor AIC em vez de BIC (mostrado acima)."
)
cat(justificativa, "\n\n")

# Mostrar coeficientes robustos do modelo final (se disponíveis)
if (!is.null(modelo_final_info$model)) {
  cat("===== Coeficientes (robustos por cluster) do modelo final =====\n")
  print(modelo_final_info$coefs)
} else {
  cat("Modelo final não foi estimado (NULL). Verifique seleção/observações).\n")
}

# ==========================================================
# ℹ️ Também exibimos o melhor por AIC para referência
# ==========================================================
cat("\n===== Modelo com melhor AIC (referência) =====\n")
print(best_aic_row)
if (!is.na(best_aic_row$key)) print(results[[best_aic_row$key]]$coefs)

cat("\n\nScript concluído. Revise as variáveis finalizadas e a justificativa acima.\n")

