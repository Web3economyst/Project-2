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
  "consumo_eletrico_sistemas_esgoto"    # FN041
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
# 🔹 TESTE DE COLINEARIDADE (VIF) — AJUSTADO
# ==========================================================
cat("\nCalculando Fatores de Inflação da Variância (VIF) - Ajustado...\n")

# 1. Definir variáveis para REMOVER (Causadoras de alto VIF)
vars_remover <- c("despesa_exploracao", "arrecadacao_total")

# 2. Criar lista filtrada de variáveis independentes
# setdiff remove os itens de 'vars_remover' da lista 'variaveis_existentes'
variaveis_vif <- setdiff(variaveis_existentes, vars_remover)

cat("Variáveis removidas do cálculo:", paste(vars_remover, collapse = ", "), "\n")
cat("Total de variáveis mantidas:", length(variaveis_vif), "\n")

# 3. Preparar o dataset apenas com as variáveis mantidas
df_vif <- dados[, c(dependente, variaveis_vif)]
df_vif <- df_vif[complete.cases(df_vif), ]

# 4. Transformação Log (Log1p)
num_cols_vif <- names(df_vif)[sapply(df_vif, is.numeric)]
num_cols_vif <- setdiff(num_cols_vif, dependente)

for (col in num_cols_vif) {
  if (all(df_vif[[col]] >= 0, na.rm = TRUE)) {
    df_vif[[col]] <- log1p(df_vif[[col]])
  }
}

# 5. Fórmula atualizada (usando apenas variaveis_vif)
formula_vif <- as.formula(paste(dependente, "~", paste(variaveis_vif, collapse = " + ")))
modelo_vif <- tryCatch(lm(formula_vif, data = df_vif), error = function(e) NULL)

if (!is.null(modelo_vif)) {
  vif_vals <- vif(modelo_vif)
  vif_df <- tibble(variavel = names(vif_vals), VIF = as.numeric(vif_vals)) %>%
    arrange(desc(VIF))
  
  cat("\n--- Tabela VIF (Resultados Ajustados) ---\n")
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
  
  # Gráfico atualizado
  print(ggplot(vif_df, aes(x = reorder(variavel, VIF), y = VIF, fill = VIF)) +
          geom_col() +
          coord_flip() +
          geom_text(aes(label = sprintf("%.2f", VIF)), hjust = -0.2, size = 3) +
          scale_fill_viridis(option = "plasma", direction = -1) +
          theme_minimal(base_size = 11) +
          labs(title = "📊 VIF Ajustado ",
               subtitle = paste("Número de Condição:", round(cond_number, 2)),
               x = "Variável", y = "VIF") +
          theme(axis.text.y = element_text(size = 8)))
  
} else {
  cat("⚠️ Falha no cálculo de VIF (possível singularidade).\n")
}

# ==========================================================
# 🔹 SELEÇÃO AUTOMÁTICA DE VARIÁVEIS (Stepwise AIC/BIC)
# ==========================================================

# --- 1. Definição das Variáveis (Pós-VIF) ---
# Removemos 'despesa_exploracao' e 'arrecadacao_total' para evitar colinearidade
variaveis_independentes <- c(
  "receita_operacional_direta_esgoto",
  "credito_areceber",
  "despesa_pessoal",
  "despesa_produto_quimico",
  "despesa_energia",
  "despesa_servico_terceiro",
  "investimento_total_prestador",
  "investimento_total_municipio",
  "investimento_total_estado",
  "consumo_eletrico_sistemas_esgoto"
)

# Garantir que a variável dependente esteja definida
if (!exists("dependente")) dependente <- "populacao_atendida_esgoto"

cat("Variáveis iniciais para seleção:", length(variaveis_independentes), "\n")
print(variaveis_independentes)


# ==========================================================
# 🔥 Preparar painel para seleção/modelos (CORRIGIDO)
# ==========================================================
cols_model <- c("id_municipio", "ano", dependente, variaveis_independentes)
cols_model <- intersect(cols_model, names(dados))
df_model <- dados[, cols_model]
df_model <- df_model %>% filter(!is.na(id_municipio), !is.na(ano))

# log1p nas numéricas (exceto id/ano/dependente)
num_cols_model <- names(df_model)[sapply(df_model, is.numeric)]
num_cols_model <- setdiff(num_cols_model, c("id_municipio", "ano", dependente))

for (col in num_cols_model) {
  # Cria uma máscara para identificar valores válidos (não-NA e >= 0)
  mask_validos <- !is.na(df_model[[col]]) & df_model[[col]] >= 0
  
  # Aplica log1p APENAS nos valores válidos
  if (any(mask_validos)) {
    df_model[[col]][mask_validos] <- log1p(df_model[[col]][mask_validos])
  }
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
  
  # Se logLik estiver disponível (alguns métodos plm não fornecem logLik padrão)
  if (!is.na(ll) && !is.na(k) && !is.na(n)) {
    aic <- -2 * ll + 2 * k
    bic <- -2 * ll + log(n) * k
    return(list(AIC = aic, BIC = bic, logLik = ll))
  }
  
  # Fallback: Cálculo via RSS (Soma dos Quadrados dos Resíduos)
  res <- tryCatch(residuals(plm_model), error = function(e) NA)
  if (is.na(res[1])) return(list(AIC = NA, BIC = NA, logLik = NA))
  
  rss <- sum(res^2, na.rm = TRUE)
  # Fórmula aproximada de AIC/BIC para mínimos quadrados
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
  
  # --- Forward ---
  if (direction == "forward") {
    selected <- c(); remaining <- vars; improved <- TRUE
    while (improved && length(remaining) > 0) {
      improved <- FALSE
      current_mod <- fit_model(selected); current_ic <- get_ic_val(current_mod)
      
      ic_add <- sapply(remaining, function(v) get_ic_val(fit_model(c(selected, v))))
      
      if (all(is.infinite(ic_add))) break
      best <- which.min(ic_add)
      
      if (ic_add[best] + 1e-8 < current_ic) {
        selected <- c(selected, names(ic_add)[best])
        remaining <- setdiff(remaining, names(ic_add)[best])
        improved <- TRUE
      }
    }
    return(selected)
  }
  
  # --- Backward ---
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
  
  # --- Both (Stepwise) ---
  if (direction == "both") {
    selected <- c(); remaining <- vars; improved_outer <- TRUE
    while (improved_outer) {
      improved_outer <- FALSE
      
      # 1. Forward step
      ic_add <- if (length(setdiff(vars, selected))==0) numeric(0) else sapply(setdiff(vars, selected), function(v) get_ic_val(fit_model(c(selected, v))))
      
      if (length(ic_add) > 0 && !all(is.infinite(ic_add))) {
        current_mod <- fit_model(selected); current_ic <- get_ic_val(current_mod)
        best_add <- which.min(ic_add)
        if (ic_add[best_add] + 1e-8 < current_ic) {
          selected <- c(selected, names(ic_add)[best_add])
          improved_outer <- TRUE
        }
      }
      
      # 2. Backward prune
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
  
  if (is.null(mod)) return(list(AIC=NA, BIC=NA))
  
  icvals <- calc_ic_plm(mod)
  
  # robust clustered SE
  vcov_clust <- tryCatch(vcovHC(mod, type = "HC1", cluster = "group"), error = function(e) NULL)
  coefs_rob <- if (!is.null(vcov_clust)) tryCatch(lmtest::coeftest(mod, vcov. = vcov_clust), error = function(e) NULL) else NULL
  
  list(model = mod, vars = varset, AIC = icvals$AIC, BIC = icvals$BIC, coefs = coefs_rob, formula = formula(mod))
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
# 🔎 Resumo final organizado
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

# Criar coluna formatada
resumo$AIC_BIC <- paste0(
  "AIC=", ifelse(is.na(resumo$AIC), "NA", sprintf("%.2f", resumo$AIC)),
  " | BIC=", ifelse(is.na(resumo$BIC), "NA", sprintf("%.2f", resumo$BIC))
)

cat("\n=== Resumo geral das seleções e ICs (AIC e BIC lado a lado) ===\n")
cols_print <- c("key", "model_type", "direction", "ic_used", "n_vars", "vars", "AIC_BIC")
print(as.data.frame(resumo)[ , cols_print, drop = FALSE])


# ==========================================================
# 🔍 Identificar melhores modelos
# ==========================================================
best_aic_row <- resumo %>% filter(!is.na(AIC)) %>% arrange(AIC) %>% slice(1)
best_bic_row <- resumo %>% filter(!is.na(BIC)) %>% arrange(BIC) %>% slice(1)

cat("\nMelhor por AIC:\n"); print(best_aic_row)
cat("\nMelhor por BIC:\n"); print(best_bic_row)

# Carregar melhor modelo por BIC
best_bic_key <- best_bic_row$key[1]
modelo_final_info <- results[[best_bic_key]]

cat("\n\n===== MODELO FINAL SUGERIDO (por BIC) =====\n")
cat("Chave:", best_bic_key, "\n")
cat("Estimador (within=random):", best_bic_row$model_type[1], "\n")
cat("Método de seleção:", best_bic_row$direction[1], "com critério", best_bic_row$ic_used[1], "\n")
cat("Variáveis selecionadas:", best_bic_row$vars[1], "\n")
cat("Número de variáveis:", best_bic_row$n_vars[1], "\n")
cat("BIC:", best_bic_row$BIC[1], " | AIC:", best_bic_row$AIC[1], "\n\n")

# Justificativa
justificativa <- paste0(
  "Justificativa: Este modelo foi escolhido porque apresentou o menor BIC entre todas as combinações.\n",
  "O BIC prioriza a parcimônia (menos variáveis), reduzindo o risco de overfitting em comparação ao AIC.\n"
)
cat(justificativa, "\n\n")

# Mostrar coeficientes
if (!is.null(modelo_final_info$model)) {
  cat("===== Coeficientes (robustos por cluster) do modelo final =====\n")
  print(modelo_final_info$coefs)
} else {
  cat("Modelo final não foi estimado (NULL).\n")
}

cat("\n\nScript concluído.\n")

# ==========================================================
# 🏆 RODAR MODELO FINAL E DIAGNÓSTICOS
# ==========================================================

# 1. Definir as variáveis vencedoras (Baseado na seleção BIC Within)
vars_final <- c("despesa_energia", 
                "despesa_pessoal", 
                "investimento_total_municipio", 
                "consumo_eletrico_sistemas_esgoto")

# 2. Construir fórmula
f_final <- as.formula(paste(dependente, "~", paste(vars_final, collapse = " + ")))
cat("📝 Fórmula Final: "); print(f_final)

# 3. Ajustar os modelos (FE e RE) para comparação
# Usamos o mesmo painel limpo da etapa anterior (painel_base)
modelo_fixed  <- plm(f_final, data = painel_base, model = "within")
modelo_random <- plm(f_final, data = painel_base, model = "random")

# ==========================================================
# 📊 TESTES DE DIAGNÓSTICO
# ==========================================================

cat("\n--- 1. Teste de Hausman (Fixo vs Aleatório) ---\n")
# H0: Modelo Aleatório é consistente (preferível). 
# H1: Modelo Aleatório é inconsistente (usar Fixo).
hausman <- phtest(modelo_fixed, modelo_random)
print(hausman)

if(hausman$p.value < 0.05) {
  cat("✅ Resultado: p < 0.05. Rejeita-se H0.\n")
  cat("👉 Recomendação: O modelo de EFEITOS FIXOS é o mais adequado estatisticamente.\n")
  modelo_oficial <- modelo_fixed
} else {
  cat("⚠️ Resultado: p >= 0.05. Não se rejeita H0.\n")
  cat("👉 Recomendação: O modelo de EFEITOS ALEATÓRIOS pode ser usado.\n")
  modelo_oficial <- modelo_random
}

# ==========================================================
# 🔹 TESTE F (Significância dos Efeitos Individuais)
# ==========================================================
cat("\n--- 2. Teste F para Efeitos Individuais (Fixed Effects vs Pooled OLS) ---\n")
cat("H0: Todos os interceptos (efeitos fixos) são iguais a zero (Pooled OLS é adequado).\n")
cat("H1: Os efeitos fixos são significativos (Existe heterogeneidade entre municípios).\n\n")

# 1. É necessário ajustar o modelo Pooled (agrupado) com a mesma fórmula final para comparar
modelo_pooled_final <- plm(f_final, data = painel_base, model = "pooling")

# 2. Calcular o Teste F (Comparando o modelo Fixed com o Pooled)
# O 'modelo_fixed' já deve ter sido gerado no bloco anterior
teste_f <- pFtest(modelo_fixed, modelo_pooled_final)
print(teste_f)

# 3. Interpretação Automática
if (!is.na(teste_f$p.value) && teste_f$p.value < 0.05) {
  cat("\n✅ Resultado: p-valor < 0.05. Rejeita-se H0.\n")
  cat("👉 Conclusão: Existem diferenças significativas e não observadas entre os municípios.\n")
  cat("   O modelo POOLED OLS é viesado. Deve-se usar Painel (Fixos ou Aleatórios).\n")
} else {
  cat("\n⚠️ Resultado: p-valor >= 0.05. Não se rejeita H0.\n")
  cat("👉 Conclusão: Os municípios se comportam de forma muito parecida.\n")
  cat("   O modelo POOLED OLS simples é suficiente para estes dados.\n")
}

# ==========================================================
# 📑 Mais visualizações:
# ==========================================================
# ==========================================================
# 🏆 REGRESSÕES FINAIS E DIAGNÓSTICO
# ==========================================================

# 1. Definição das variáveis vencedoras (Seleção BIC)
vars_final <- c("despesa_energia", 
                "despesa_pessoal", 
                "investimento_total_municipio", 
                "consumo_eletrico_sistemas_esgoto")

# 2. Fórmula Final
f_final <- as.formula(paste(dependente, "~", paste(vars_final, collapse = " + ")))

# 3. Estimativa dos três modelos para comparação
modelo_pooled <- plm(f_final, data = painel_base, model = "pooling")
modelo_fixed  <- plm(f_final, data = painel_base, model = "within")
modelo_random <- plm(f_final, data = painel_base, model = "random")

# ==========================================================
# 1. Regressões - Modelos de Painel (Pooled, FE, RE)
# ==========================================================
cat("\n==========================================================\n")
cat("🔹 1. Regressões - Modelos de Painel (Pooled, FE, RE)\n")
cat("==========================================================\n")

# Teste F (Chow Test) - Decide entre Pooled e Efeito Fixo
cat("\n--- Teste F (Pooled OLS vs Efeitos Fixos) ---\n")
teste_f <- pFtest(modelo_fixed, modelo_pooled)
print(teste_f)
if(teste_f$p.value < 0.05) {
  cat("✅ p < 0.05: Pooled é rejeitado. Há efeitos específicos de cada município.\n")
} else {
  cat("⚠️ p >= 0.05: Pooled é suficiente.\n")
}

# Teste de Hausman - Decide entre Efeito Fixo e Aleatório
cat("\n--- Teste de Hausman (Fixo vs Aleatório) ---\n")
hausman <- phtest(modelo_fixed, modelo_random)
print(hausman)

modelo_vencedor <- modelo_pooled # fallback
nome_modelo <- "Pooled OLS"

if (teste_f$p.value < 0.05) {
  if (hausman$p.value < 0.05) {
    cat("✅ p < 0.05: Rejeita-se H0 (Aleatório). O modelo CONSISTENTE é o de EFEITOS FIXOS.\n")
    modelo_vencedor <- modelo_fixed
    nome_modelo <- "Efeitos Fixos (Fixed Effects)"
  } else {
    cat("👉 p >= 0.05: Não se rejeita H0. O modelo Aleatório é preferível.\n")
    modelo_vencedor <- modelo_random
    nome_modelo <- "Efeitos Aleatórios (Random Effects)"
  }
}

# ==========================================================
# 2. Modelos de Painel – Efeito Fixo
# ==========================================================
cat("\n\n==========================================================\n")
cat("🔹 2. Modelos de Painel – Efeito Fixo (Modelo Analisado)\n")
cat("==========================================================\n")

# Exibe o R-quadrado do modelo Fixed (Within)
r2_fe <- summary(modelo_fixed)$r.squared
cat(paste("R-Squared (Within):", round(r2_fe["rsq"], 4), "\n"))
cat(paste("Adj. R-Squared:   ", round(r2_fe["adjrsq"], 4), "\n"))

# ==========================================================
# 3. T teste dos coeficientes
# ==========================================================
cat("\n\n==========================================================\n")
cat("🔹 3. T teste dos coeficientes (Com Erros-Padrão Robustos)\n")
cat("==========================================================\n")
cat("Nota: Utilizando matriz de covariância robusta de Arellano (Cluster por Município)\n")
cat("      Isso corrige heterocedasticidade e autocorrelação serial.\n\n")

# Calcula os erros robustos
vcov_robusta <- vcovHC(modelo_fixed, method = "arellano", type = "HC1", cluster = "group")

# Exibe a tabela final formatada (Estimativa, Erro Padrão, Valor t, Pr(>|t|))
t_teste_final <- lmtest::coeftest(modelo_fixed, vcov. = vcov_robusta)
print(t_teste_final)

cat("\n==========================================================\n")
cat("🔚 Fim da Análise.\n")
