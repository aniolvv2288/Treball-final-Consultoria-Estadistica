
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
  "sigles", "independentista", "quota"
)



modelsideologia <- function(eleccio_actual, dadesquota, vars_excloure) {
  
  cat("ELECCIÓ:", eleccio_actual)
  
  df <- dadesquota %>%
    filter(eleccio == eleccio_actual) %>%
    drop_na(ideologia) %>%
    select(-all_of(vars_excloure)) %>%
    filter(complete.cases(.))
  
  df$group <- df$seccio
  
  if(nrow(df) < 50) {
    warning(paste("Massa poques observacions per", eleccio_actual))
    return(NULL)
  }
  
  split <- group_initial_split(
    df,
    group = group,
    prop = 0.8
  )
  train <- training(split)
  test  <- testing(split)
  
  Xtrain <- train %>% select(-ideologia, -group, -seccio)
  Ytrain <- train$ideologia
  
  Xtest <- test %>% select(-ideologia, -group, -seccio)
  Ytest <- test$ideologia
  
  # RF base
  rf <- randomForest(
    x = Xtrain,
    y = Ytrain,
    xtest = Xtest,
    ytest = Ytest,
    importance = TRUE,
    seed = 1714
  ) #si es fa plot d'aixo s'observa que 100 trees son suficients
  
  tuning <- tuneRF(
    x = Xtrain,
    y = Ytrain,
    nTreeTry = 100,
    stepFactor = 1.5,
    mtryStart = 5,
    improve = 0.01,
    xtest = Xtest,
    ytest = Ytest,
    importance = TRUE,
    seed = 1714
  )
  
  millormtry <- tuning[which.min(tuning[, 2]), 1]
  
  model <- ranger(
    x = Xtrain,
    y = Ytrain,
    num.trees = 100,
    mtry = millormtry,
    probability = TRUE,
    seed = 1714
  )
  
  X_shap <- Xtrain %>% sample_n(min(500, nrow(Xtrain)))
  
  run_shap <- function(classe) {
    
    pred_wrapper <- function(object, newdata) {
      predict(object, newdata)$predictions[, classe]
    }
    
    shapvals <- fastshap::explain(
      object = model,
      X = X_shap,
      pred_wrapper = pred_wrapper,
      nsim = 10,
      adjust = TRUE
    )
    
    shaplong <- shapvals %>%
      as.data.frame() %>%
      mutate(id = row_number()) %>%
      pivot_longer(-id, names_to = "variable", values_to = "shap")
    
    # Plot importància
    p1 <- shaplong %>%
      group_by(variable) %>%
      summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE)) %>%
      arrange(desc(mean_abs_shap)) %>%
      slice_head(n = 20) %>%
      ggplot(aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap)) +
      geom_col(fill = "firebrick") +
      coord_flip() +
      labs(
        title = paste("SHAP Global —", classe, "-", eleccio_actual),
        x = "Variable",
        y = "|SHAP| mitjà"
      ) +
      theme_minimal(base_size = 14)
    
    # Plot cascada
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
        title = paste("SHAP Waterfall —", classe, "-", eleccio_actual),
        x = "Contribució acumulada SHAP",
        y = "Variable",
        fill = "Direcció"
      ) +
      theme_minimal(base_size = 14)
    
    list(shap = shaplong, plot_global = p1, plot_waterfall = p2)
  }
  
  list(
    eleccio = eleccio_actual,
    esquerra = run_shap("Esquerra"),
    dreta = run_shap("Dreta")
  )
}

llistaeleccions <- levels(dadesquota$eleccio)

resultats <- purrr::map(
  llistaeleccions,
  ~ modelsideologia(.x, dadesquota, vars_excloure)
)


for(eleccio in 1:5){
  resultats[[eleccio]]$esquerra$plot_global
  resultats[[eleccio]]$esquerra$plot_waterfall
  
  resultats[[eleccio]]$dreta$plot_global
  resultats[[eleccio]]$dreta$plot_waterfall
}

