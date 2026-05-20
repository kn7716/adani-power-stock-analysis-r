# adani-power-stock-analysis-r
Complete R project for Adani Power stock analysis including financial data acquisition, visualization, ARIMA forecasting, and algorithmic trading strategies
# Adani Power Stock Analysis & Algorithmic Trading (R Project)

## Overview
This project is a complete financial analytics and algorithmic trading project developed in R using Adani Power Limited stock data from Yahoo Finance.

The project covers:
- Financial data acquisition
- Data cleaning & preprocessing
- Advanced data visualization
- Time series forecasting
- Technical indicators
- Algorithmic trading strategies
- Performance analysis

Ticker Used:
- ADANIPOWER.NS (NSE India)

---

## Features

### Part 1: Financial Data Acquisition & Handling
- Download stock market data using Yahoo Finance API
- Handle CSV and Excel files
- Data cleaning and preprocessing
- Missing value handling
- Feature engineering

### Data Visualization
- Closing price trend analysis
- Volume analysis
- Candlestick-style charts
- Histogram of returns
- Moving averages visualization
- Boxplots of returns

### Time Series Analysis
- Trend analysis
- STL decomposition
- Stationarity testing (ADF Test)
- ARIMA modeling
- Forecasting future stock prices

### Algorithmic Trading
Implemented trading strategies:
- SMA Crossover Strategy
- RSI Strategy
- MACD Strategy
- Bollinger Bands Analysis

### Performance Metrics
- Total Returns
- Annual Returns
- Volatility
- Sharpe Ratio
- Maximum Drawdown
- Win Rate

---

## Technologies Used

- R Programming
- quantmod
- tidyverse
- ggplot2
- forecast
- TTR
- PerformanceAnalytics
- lubridate

---

## Output Files

The project generates:
- Adani_Power_Stock_Data.csv
- Adani_Power_Stock_Data.xlsx
- Adani_Power_Strategy_Metrics.csv
- Adani_Power_Complete_Analysis.csv

---

## Project Structure

```bash
├── Adani power data visualization.R
├── README.md
├── outputs/
│   ├── Adani_Power_Stock_Data.csv
│   ├── Adani_Power_Stock_Data.xlsx
│   ├── Adani_Power_Strategy_Metrics.csv
│   └── Adani_Power_Complete_Analysis.csv
