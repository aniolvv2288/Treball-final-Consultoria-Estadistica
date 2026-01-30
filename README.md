# Electoral behaviour analysis in Catalonia: evolution and influence socioeconomic and demographic factors

This repository contains the code, data workflows, and documentation for our final project in Statistical Consulting at the Universitat Autònoma de Barcelona (UAB), including data analysis, linear and mixed modeling, machine learning and interactive visualization with Shiny.

## 📖 Project Overview

This project analyses electoral behaviour in Catalonia using detailed voting results from Spanish general elections combined with socioeconomic and demographic data. By working at the census‑section level, it uncovers territorial patterns that are not visible in more aggregated analyses.
The goal is to understand how political support is distributed across the territory, how it changes over time, and which social and economic factors help explain these differences. The project integrates data processing, spatial analysis, and statistical modelling to provide a clearer picture of the relationship between social context and voting outcomes.

## 🔑 Key Components

- **Data Cleaning & Preprocessing:** R scripts for importing, cleaning, formatting, and preparing data.
- **Exploratory Data Analysis (EDA):** Multivariate descriptive analysis (tables, maps, CA) to explore socioeconomic and electoral patterns.
- **Linear and Mixed Models:** Application of multivariate regression and hierarchical mixed models (Poisson/NegBin) with random effects for ABS and health regions.
- **Machine Learning and SHAP:** Random Forest and XGBoost models. SHAP values shown to interpret effects on the independent variable.
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

