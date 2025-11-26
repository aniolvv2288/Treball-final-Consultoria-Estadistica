# Multivariate and Spatial Analysis of Mortality in Catalonia using INLA

This repository contains the code, data workflows, and documentation for our final project in Statistical Consulting at the Universitat Autònoma de Barcelona (UAB), including data analysis, INLA modeling, and interactive visualization with Shiny.

## 📖 Project Overview

Posar text

## 🔑 Key Components

- **Data Cleaning & Preprocessing:** R scripts for importing, cleaning, formatting, and preparing mortality data (ABS, regions, sex, age groups, socioeconomic index).
- **Data Imputation:** Strategies to handle missing values or inconsistencies in demographic and socioeconomic variables (e.g., multiple imputation with `mice`).
- **Outliers Exploration:** Identification and evaluation of extreme values in mortality by ABS or age groups, using scatterplots and boxplots.
- **Exploratory Data Analysis (EDA):** Multivariate descriptive analysis (tables, heatmaps, PCA, clustering) to explore mortality patterns by sex, age, and socioeconomic deprivation.
- **Multivariate & Mixed Models:** Application of multivariate regression and hierarchical mixed models (Poisson/NegBin) with random effects for ABS and health regions.
- **Spatial Modeling with INLA:** Construction of spatial mortality models with INLA using the BYM2 technique for ABS and SPDE for continuous fields, incorporating temporal effects (RW1/RW2).
- **Reproducibility:** Modular R scripts, complete report in R Markdown, version control via GitHub, and Shiny application to share interactive results (maps, trends, tables).


## 📂 Structure

- `/data`: Raw and processed datasets (with `.gitignore` for sensitive files)
- `/scripts`: R scripts for each analysis step
- `/output`: Maps, tables, and results
- `/docs`: POSAR TREBALL FINAL QUAN ACABEM

## 👥 Authors

- **Aniol Vilamala Vidal**  
- **Jordi Anguera Costa**  
- **Èric Segon Salmerón**  
- **Daniel Jiménez López**

📍 *Universitat Autònoma de Barcelona (UAB)*

