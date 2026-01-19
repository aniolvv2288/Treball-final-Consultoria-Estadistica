
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
# ----- MODEL BACKWARD PER PARTIT -----
################################################################################

library(car)
library(MASS)

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

library(lme4)
library(lmerTest)

models_mixtes <- list()

for (partit in partits) {
  
  cat("\n==================================\n")
  cat("Models mixtes per al partit:", partit, "\n")
  cat("==================================\n")
  
  dades_partit <- dades %>%
    filter(sigles == partit) %>%
    droplevels()
  
  # MODEL 1: interceptes aleatoris per secció i elecció
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
  
  # MODEL 3: només secció
  model3 <- lmer(
    logit_pct_vot ~ `Edat Mitjana` + `Població` +
      `Mida mitjana llar` + `Renda mitjana uc` +
      `índex Gini` + `Població no espanyola` +
      (1 | seccio),
    data = dades_partit
  )
  
  # MODEL 4: només elecció
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
  
  mods <- models_mixtes[[partit]]
  
  df_res <- data.frame(
    model = character(),
    AIC = numeric(),
    BIC = numeric(),
    ICC = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (nom_model in names(mods)) {
    m <- mods[[nom_model]]
    df_res <- rbind(
      df_res,
      data.frame(
        model = nom_model,
        AIC = AIC(m),
        BIC = BIC(m),
        ICC = icc_from_lmer(m)
      )
    )
  }
  
  print(df_res)
  resultats_models[[partit]] <- df_res
}


################################################################################
# ----- VALIDACIÓ DEL MODEL ESCOLLIT -----
################################################################################

# anàlisi i validació del model escollit donats els resultats anteriors


################################################################################
# ----- VISUALITZACIÓ TAULES PEL MODEL ESCOLLIT -----
################################################################################

library(gtsummary)

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
    modify_caption(paste("Resultats del model mixt per a", partit))
  
  taules_models[[partit]] <- tbl
  
  cat("\n=============================\n")
  cat("Taula per al partit:", partit, "\n")
  cat("=============================\n")
  print(tbl)
}

## AMB PROBABILITAT

library(gtsummary)

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
    modify_caption(paste("Resultats del model mixt (probabilitats) per a", partit))
  
  taules_models[[partit]] <- tbl
  
  cat("\n=============================\n")
  cat("Taula per al partit:", partit, "\n")
  cat("=============================\n")
  print(tbl)
}

