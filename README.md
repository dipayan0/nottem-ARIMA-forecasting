# Impact of Transformations in Modeling and Forecasting with ARIMA

This repository contains the full assignment and code for the **Time Series Analysis and Forecasting** course (WM-ASDS10, Fall 2023) at Jahangirnagar University, submitted by **Dipayan Bhadra**.

## 📘 Project Overview

The goal of this project is to explore how different transformations—seasonal differencing, logarithmic, and BoxCox—impact ARIMA model performance and forecasting accuracy. The dataset used is `nottem`, which contains monthly average air temperatures at Nottingham Castle from 1920 to 1939.

## 📊 Methodology

1. **Data Exploration**:
   - Time plot, ACF, PACF, sub-series plot, and boxplot
   - Seasonal patterns and stationarity checks

2. **Modeling**:
   - **M1**: SARIMA on raw data
   - **M2**: SARIMA on log-transformed data
   - **M3**: SARIMA on BoxCox-transformed data
   - **M4**: auto.arima model selection

3. **Diagnostics**:
   - Residual checks: Ljung-Box, McLeod-Li, Shapiro-Wilk
   - Model selection criteria: AIC, BIC, AICc

4. **Forecasting**:
   - Forecasts on test data (1934–1939)
   - Accuracy metrics: MSE, RMSE, MAE, MPE, MAPE
   - Final 10-step forecast using best model

## 📁 Contents

- `code/`: R scripts for each model and forecasting
- `report/`: Full PDF assignment with tables, equations, and plots
- `data/`: Optional CSV version of the `nottem` dataset
- `README.md`: Project summary and instructions

## 📦 Requirements

- R (≥ 4.0)
- Packages: `forecast`, `tseries`, `ggplot2`, `dplyr`

## 📈 Results Summary

| Model | AIC       | BIC         | Residuals Normal? | Best Accuracy |
|-------|-----------|-------------|-------------------|----------------|
| M1    | 729.81    | 751.16      |       ✅         |       ❌       |
| M2    | -457.82   | -439.52     |       ❌         |       ✅       |
| M3    | -1095.46  | -1077.16    |       ❌         |       ✅       |
| M4    | 733.99    | 746.19      |       ✅         |       ❌       |

> Final model selected: **M3 (BoxCox-transformed SARIMA)**

## 📜 License

This project is shared for educational and portfolio purposes. Please cite appropriately if reused.

## 🙋 Author

**Dipayan Bhadra**  
PM-ASDS, Jahangirnagar University  
Course: WM-ASDS10, Fall 2023  
Instructor: Prof. Dr. Rumana Rois
