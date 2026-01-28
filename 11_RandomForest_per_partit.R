
set.seed(1714)

dades <- readRDS("dades_finals.rds")

dadesquota <- dades %>%
  group_by(seccio, any, eleccio) %>%
  mutate(
    total_vots = sum(vots, na.rm = TRUE),
    quota = ifelse(total_vots > 0, vots / total_vots, NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(quota))

# Variables a excloure del model

vars_excloure <- c(
  "vots", "total_vots",
  "any", "mes", "provincia", "districte",
  "Municipi", "Codi_municipi",
  "nom_partit", "eleccio",
  "sigles", "independentista", "ideologia"
)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = FALSE
)


modelspartits <- function(eleccio_actual, dadesquota, vars_excloure, ctrl) {
  
  cat("\n===== ELECCIÓ:", eleccio_actual, "=====\n")
  
  dades_eleccio <- dadesquota %>%
    filter(as.character(eleccio) == as.character(eleccio_actual))
  
  sigles_llista <- levels(dades_eleccio$sigles)
  
  map(sigles_llista, function(sigla_actual) {
    
    cat("  -> SIGLA:", sigla_actual, "\n")
    
    df <- dades_eleccio %>%
      filter(sigles == sigla_actual) %>%
      select(-all_of(vars_excloure)) %>%
      drop_na(quota) %>%
      filter(complete.cases(.))
    
    if (nrow(df) < 50) {
      warning(paste("Massa poques observacions per", eleccio_actual, sigla_actual))
      return(NULL)
    }
    
    df$group <- df$seccio
    
    #-----------------------
    # Train / Test split
    #-----------------------
    split <- group_initial_split(
      df,
      group = group,
      prop = 0.8
    )
    train <- training(split)
    test  <- testing(split)
    Xtrain <- train %>% select(-quota, -group, -seccio)
    Ytrain <- train$quota
    
    Xtest <- test %>% select(-quota, -group, -seccio)
    Ytest <- test$quota
    
    train_df <- bind_cols(Xtrain, quota = Ytrain)
    
    #-----------------------
    # Grid de tuning
    #-----------------------
    p <- ncol(Xtrain)
    grid <- expand.grid(
      mtry = unique(round(seq(2, sqrt(p), length.out = 5)))
    )
    
    #-----------------------
    # Caret RF amb CV
    #-----------------------
    rf_caret <- train(
      quota ~ .,
      data = train_df,
      method = "rf",
      trControl = ctrl,
      tuneGrid = grid,
      ntree = 100,
      importance = TRUE
    )
    
    best_mtry <- 30
    print(rf_caret$bestTune)
    #-----------------------
    # Model final (ranger)
    #-----------------------
    rf <- ranger(
      x = Xtrain,
      y = Ytrain,
      num.trees = 100,
      mtry = best_mtry,
      seed = 1714
    )
    
    #-----------------------
    # Avaluació test
    #-----------------------
    preds <- predict(rf, Xtest)$predictions
    
    rmse <- RMSE(preds, Ytest)
    mae  <- MAE(preds, Ytest)
    r2   <- R2(preds, Ytest)
    
    metrics_row <- tibble(
      eleccio = eleccio_actual,
      sigla = sigla_actual,
      n_train = nrow(train),
      n_test = nrow(test),
      best_mtry = best_mtry,
      RMSE = rmse,
      MAE = mae,
      R2 = r2
    )
    
    #-----------------------
    # SHAP
    #-----------------------
    X_shap <- Xtrain %>% sample_n(min(500, nrow(Xtrain)))
    
    pred_wrapper <- function(object, newdata) {
      predict(object, newdata)$predictions
    }
    
    shapvals <- fastshap::explain(
      object = rf,
      X = X_shap,
      pred_wrapper = pred_wrapper,
      nsim = 10,
      adjust = TRUE
    )
    
    shaplong <- shapvals %>%
      as.data.frame() %>%
      mutate(id = row_number()) %>%
      pivot_longer(-id, names_to = "variable", values_to = "shap")
    
    # --- SHAP GLOBAL ---
    p1 <- shaplong %>%
      group_by(variable) %>%
      summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE)) %>%
      arrange(desc(mean_abs_shap)) %>%
      slice_head(n = 20) %>%
      ggplot(aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap)) +
      geom_col(fill = "firebrick") +
      coord_flip() +
      labs(
        title = paste("SHAP Global —", sigla_actual, "-", eleccio_actual),
        x = "Variable socioeconòmica",
        y = "Impacte mitjà |SHAP| sobre quota"
      ) +
      theme_minimal(base_size = 14)
    
    # --- SHAP WATERFALL ---
    shapcascada <- shaplong %>%
      group_by(variable) %>%
      summarise(mean_shap = mean(shap, na.rm = TRUE)) %>%
      arrange(desc(mean_shap)) %>%
      mutate(
        variable = factor(variable, levels = variable),
        cum_effect = cumsum(mean_shap),
        xmin = lag(cum_effect, default = 0),
        xmax = cum_effect
      )
    
    p2 <- ggplot(shapcascada) +
      geom_rect(aes(
        xmin = xmin,
        xmax = xmax,
        ymin = as.numeric(variable) - 0.4,
        ymax = as.numeric(variable) + 0.4,
        fill = mean_shap > 0
      )) +
      scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
      scale_y_continuous(
        breaks = seq_along(shapcascada$variable),
        labels = shapcascada$variable
      ) +
      labs(
        title = paste("SHAP Waterfall —", sigla_actual, "-", eleccio_actual),
        x = "Contribució acumulada SHAP sobre quota",
        y = "Variable",
        fill = "Direcció"
      ) +
      theme_minimal(base_size = 14)
    
    list(
      metrics = metrics_row,
      shap = shaplong,
      plot_global = p1,
      plot_waterfall = p2
    )
  })
}



llistaeleccions <- levels(dadesquota$eleccio)

resultats <- map(
  llistaeleccions,
  ~ modelspartits(.x, dadesquota, vars_excloure, ctrl)
)

taulametriques <- resultats %>%
  flatten() %>%
  map("metrics") %>%
  bind_rows()

taulametriques

resultats
