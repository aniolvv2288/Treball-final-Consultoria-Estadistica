library(randomForest)
library(tidyverse)
library(rsample)
library(ranger)
library(fastshap)
library(vip)
library(caret)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(fastshap)

set.seed(1714)

dades <- readRDS("dades_finals.rds")

dades_quota <- dades %>%
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
  "any", "mes", "provincia", "districte", "seccio",
  "Municipi", "Codi_municipi",
  "nom_partit", "eleccio",
  "sigles", "independentista", "quota"
)

df<-dades_quota%>%
  drop_na(ideologia) %>%
  select(-all_of(vars_excloure))
df<-df[complete.cases(df),]

# Evitar classes amb poques observacions

if(nrow(df) < 10) {
  warning("Massa poques observacions")
  return(list(model = NULL, shap = NULL))
}

split <- initial_split(df, prop = 0.8)
train <- training(split)
test  <- testing(split)

Xtrain <- train %>% select(-ideologia)
Ytrain <- train$ideologia

Xtest <- test %>% select(-ideologia)
Ytest <- test$ideologia


# Random Forest de classificació
model <- randomForest(
  x = Xtrain,
  y = Ytrain,
  xtest = Xtest,
  ytest = Ytest,
  importance = TRUE,
  seed = 1714
)
print(model)

plot(model) #amb 100 trees ja converxeig

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

# Wrapper: probabilitat classe "Esquerra"

prediccions <- function(object, newdata) {
  predict(object, newdata)$predictions[, "Esquerra"]
}

# Calcular SHAP
shapvals <- fastshap::explain(
  object = model,
  X = X_shap,
  pred_wrapper = prediccions,
  nsim = 10,
  adjust = TRUE
)

shaplong <- shapvals %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "shap")



shapabsolut <- function(shaplong, top_n = 15, title = "Importància global SHAP") {
  shaplong %>%
    group_by(variable) %>%
    summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap)) %>%
    slice_head(n = top_n) %>%
    ggplot(aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap)) +
    geom_col(fill = "firebrick") +
    coord_flip() +
    labs(
      title = title,
      x = "Variable socioeconòmica",
      y = "Impacte mitjà absolut (|SHAP|)"
    ) +
    theme_minimal(base_size = 14)
}


shapabsolut(shaplong, top_n = 20,
            title = "Factors socioeconòmics del perfil ideològic (SHAP)")

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

ggplot(shapcascada) +
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
    title = "SHAP Waterfall — Efecte acumulat sobre P(Esquerra)",
    x = "Contribució acumulada SHAP",
    y = "Variable",
    fill = "Direcció"
  ) +
  theme_minimal(base_size = 14)



# El mateix per "Dreta"

prediccions <- function(object, newdata) {
  predict(object, newdata)$predictions[, "Dreta"]
}

# Calcular SHAP
shapvals <- fastshap::explain(
  object = model,
  X = X_shap,
  pred_wrapper = prediccions,
  nsim = 10,
  adjust = TRUE
)

shaplong <- shapvals %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "shap")



shapabsolut <- function(shaplong, top_n = 15, title = "Importància global SHAP") {
  shaplong %>%
    group_by(variable) %>%
    summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE)) %>%
    arrange(desc(mean_abs_shap)) %>%
    slice_head(n = top_n) %>%
    ggplot(aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap)) +
    geom_col(fill = "firebrick") +
    coord_flip() +
    labs(
      title = title,
      x = "Variable socioeconòmica",
      y = "Impacte mitjà absolut (|SHAP|)"
    ) +
    theme_minimal(base_size = 14)
}


shapabsolut(shaplong, top_n = 20,
            title = "Factors socioeconòmics del perfil ideològic (SHAP)")

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

ggplot(shapcascada) +
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
    title = "SHAP Waterfall — Efecte acumulat sobre P(Dreta)",
    x = "Contribució acumulada SHAP",
    y = "Variable",
    fill = "Direcció"
  ) +
  theme_minimal(base_size = 14)



