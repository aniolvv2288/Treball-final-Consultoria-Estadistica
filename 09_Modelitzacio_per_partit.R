
################################################################################
# ----- INCORPORACIÓ DADES I TRANSFORMACIÓ A LOGIT -----
################################################################################

dades <- readRDS("dades_finals.rds")

dades <- dades %>%
  group_by(seccio, eleccio) %>%
  mutate(
    total_vots = sum(vots),
    pct_vot = 100 * vots / total_vots
  ) %>%
  ungroup()

dades <- dades %>%
  mutate(
    pct_vot_adj = pmin(pmax(pct_vot, 0.1), 99.9),
    logit_pct_vot = log(pct_vot_adj / (100 - pct_vot_adj))
  )

dades <- dades %>% 
  dplyr::select(
    sigles, seccio, eleccio, logit_pct_vot,
    fac_Edad_media, fac_Población, fac_Tamaño_medio_del_hogar,
    fac_renda_media_uc, fac_indice_gini, fac_poblacio_no_espanyola
  ) %>%
  dplyr::rename(
    `Edat Mitjana` = fac_Edad_media,
    `Població` = fac_Población,
    `Mida mitjana llar` = fac_Tamaño_medio_del_hogar,
    `Renda mitjana uc` = fac_renda_media_uc,
    `índex Gini` = fac_indice_gini,
    `Població no espanyola` = fac_poblacio_no_espanyola
  ) %>%
  na.omit()


################################################################################
# ----- MODEL LINEAL PER PARTIT -----
################################################################################

partits <- c("CONV./JxCAT", "CS", "CUP/PR", "ECP/SUMAR",
             "ERC", "PP", "PSC/PSOE", "VOX")

models_final <- list()

for (partit in partits) {
  
  cat("\n=============================\n")
  cat("Model per al partit:", partit, "\n")
  cat("=============================\n")
  
  dades_partit <- dades %>% filter(sigles == partit) %>% 
    dplyr::select(-sigles, -seccio)
  
  model_full <- lm(logit_pct_vot~., data = dades_partit)
  
  models_final[[partit]] <- model_full
  
  print(summary(model_full))
}


################################################################################
# ----- MODELS MIXTES PER PARTIT -----
################################################################################

models_mixtes <- list()

for (partit in partits) {
  
  cat("\n==================================\n")
  cat("Models mixtes per al partit:", partit, "\n")
  cat("==================================\n")
  
  dades_partit <- dades %>%
    filter(sigles == partit) %>%
    droplevels()
  
  # MODEL 1: iid per secció i elecció
  model1 <- lmer(
    logit_pct_vot ~ `Edat Mitjana` + `Població` +
      `Mida mitjana llar` + `Renda mitjana uc` +
      `índex Gini` + `Població no espanyola` +
      (1 | seccio) + (1 | eleccio),
    data = dades_partit
  )
  
  # MODEL 2: eleccio efecte fix + iid seccio
  model2 <- lmer(
    logit_pct_vot ~ `Edat Mitjana` + `Població` + eleccio +
      `Mida mitjana llar` + `Renda mitjana uc` +
      `índex Gini` + `Població no espanyola` +
      (1 | seccio),
    data = dades_partit
  )

  # MODEL 3: només iid secció
  model3 <- lmer(
    logit_pct_vot ~ `Edat Mitjana` + `Població` +
      `Mida mitjana llar` + `Renda mitjana uc` +
      `índex Gini` + `Població no espanyola` +
      (1 | seccio),
    data = dades_partit
  )
  
  # MODEL 4: només iid elecció
  model4 <- lmer(
    logit_pct_vot ~ `Edat Mitjana` + `Població` +
      `Mida mitjana llar` + `Renda mitjana uc` +
      `índex Gini` + `Població no espanyola` +
      (1 | eleccio),
    data = dades_partit
  )
  
  models_mixtes[[partit]] <- list(
    model1 = model1,
    model2 = model2,
    model3 = model3,
    model4 = model4
  )
}


################################################################################
# ----- AVALUACIÓ DELS MODELS PER CADA PARTIT -----
################################################################################

n_param <- function(model) {
  if (inherits(model, "lm")) {
    return(length(coef(model)))
  }
  if (inherits(model, "lmerMod")) {
    return(length(fixef(model)) + length(getME(model, "theta")))
  }
  return(NA)
}

icc_from_lmer <- function(model) {
  vc <- as.data.frame(VarCorr(model))
  var_re <- sum(vc$vcov[vc$grp != "Residual"])
  var_res <- vc$vcov[vc$grp == "Residual"]
  icc <- var_re / (var_re + var_res)
  return(icc)
}

resultats_models <- list()

for (partit in partits) {
  
  cat("\n==================================\n")
  cat("Avaluació models per al partit:", partit, "\n")
  cat("==================================\n")
  
  mods_mixtes <- models_mixtes[[partit]]
  mod_lm <- models_final[[partit]]
  
  df_res <- data.frame(
    model = character(),
    AIC = numeric(),
    BIC = numeric(),
    ICC = character(),
    n_param = numeric(),
    stringsAsFactors = FALSE
  )
  
  df_res <- rbind(
    df_res,
    data.frame(
      model = "model0",
      AIC = AIC(mod_lm),
      BIC = BIC(mod_lm),
      ICC = "-",
      n_param = n_param(mod_lm)
    )
  )
  
  for (nom_model in names(mods_mixtes)) {
    m <- mods_mixtes[[nom_model]]
    df_res <- rbind(
      df_res,
      data.frame(
        model = nom_model,
        AIC = AIC(m),
        BIC = BIC(m),
        ICC = sprintf("%.3f", icc_from_lmer(m)),
        n_param = n_param(m)
      )
    )
  }
  
  colnames(df_res)[colnames(df_res) == "n_param"] <- "# Paràm."
  colnames(df_res)[colnames(df_res) == "model"] <- "Model"
  
  print(df_res)
  resultats_models[[partit]] <- df_res
}


colors_partits <- c(
  "PSC/PSOE"    = "#E30613",
  "PP"          = "#1D4ED8",
  "ERC"         = "#FFB90F",
  "CONV./JxCAT" = "#76EEC6",
  "CUP/PR"      = "#FFD700",
  "CS"          = "#F97316",
  "VOX"         = "#00FF00",
  "ECP/SUMAR"   = "#7C3AED"
)

taules_kable <- list()

for (partit in names(resultats_models)) {
  
  df <- resultats_models[[partit]]
  
  idx_best <- which.min(df$BIC)
  
  df$`Model òptim` <- ifelse(seq_len(nrow(df)) == idx_best, "⭐", "")
  
  kbl_tbl <- df %>%
    kbl(
      caption = paste("Models per al partit:", partit),
      align = "c",
      digits = 3,
      escape = FALSE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 15
    ) %>%
    row_spec(
      idx_best,
      bold = TRUE,
      color = "white",
      background = colors_partits[partit]
    ) %>%
    add_header_above(
      c(" " = 1, "Mètriques dels models" = 4, " " = 1),
      bold = TRUE,
      background = "#f0f0f0"
    ) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(ncol(df), width = "3em") %>%
    kable_classic(html_font = "Helvetica") %>%
    footnote(
      general = "⭐ indica el model amb millor BIC.",
      general_title = "",
      footnote_as_chunk = TRUE
    )
  
  taules_kable[[partit]] <- kbl_tbl
}

taules_kable$`CONV./JxCAT`


################################################################################
# ----- VALIDACIÓ DEL MODEL ESCOLLIT -----
################################################################################

for (partit in partits) {
  
  model_sel <- models_mixtes[[partit]]$model1   # POSAR MODEL ESCOLLIT
  
  cat("\n===========================================\n")
  cat("Validació del model per al partit:", partit, "\n")
  cat("===========================================\n")
  
  # Residus vs ajustats
  plot(
    residuals(model_sel, type = "pearson") ~ fitted(model_sel),
    main = paste("Residus vs Valors ajustats —", partit),
    xlab = "Valors ajustats",
    ylab = "Residus (Pearson)"
  )
  abline(h = 0, col = "red", lty = 2)
  
  # Histograma dels residus
  hist(
    residuals(model_sel, type = "pearson"),
    breaks = 15,
    main = paste("Histograma dels residus —", partit),
    xlab = "Residus (Pearson)"
  )
  
  # QQ-plot
  qqnorm(
    residuals(model_sel, type = "pearson"),
    main = paste("QQ-plot dels residus —", partit)
  )
  qqline(residuals(model_sel, type = "pearson"), col = "red")
  
  # ACF
  acf(
    residuals(model_sel, type = "pearson"),
    main = paste("ACF dels residus —", partit)
  )
  
  # PACF
  pacf(
    residuals(model_sel, type = "pearson"),
    main = paste("PACF dels residus —", partit)
  )
}


################################################################################
# ----- VISUALITZACIÓ TAULES PEL MODEL ESCOLLIT -----
################################################################################

## AMB ODDS-RATIO

taules_models <- list()

for (partit in partits) {
  
  model_sel <- models_mixtes[[partit]]$model1 # POSAR MODEL QUE ESCOLLIM
  
  tbl <- tbl_regression(
    model_sel,
    exponentiate = TRUE,
    intercept = FALSE
  ) %>%
    modify_header(label ~ "Variable") %>%
    modify_caption(paste("Coeficients (OR) per al partit:", partit))
  
  taules_models[[partit]] <- tbl
  
  cat("\n=============================\n")
  cat("Taula per al partit:", partit, "\n")
  cat("=============================\n")
  print(tbl)
}

## AMB PROBABILITAT

taules_models <- list()

for (partit in partits) {
  
  model_sel <- models_mixtes[[partit]]$model1 # POSAR MODEL QUE ESCOLLIM
  
  tbl <- tbl_regression(
    model_sel,
    exponentiate = FALSE,   
    intercept = FALSE,
    estimate_fun = function(x) scales::percent(plogis(x), accuracy = 0.1)
  ) %>%
    modify_header(label ~ "Variable") %>%
    modify_caption(paste("Probabilitats per al partit:", partit))
  
  taules_models[[partit]] <- tbl
  
  cat("\n=============================\n")
  cat("Taula per al partit:", partit, "\n")
  cat("=============================\n")
  print(tbl)
}

