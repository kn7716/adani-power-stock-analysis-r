# =============================================================================
#                    ADANI POWER LIMITED - COMPLETE R PROJECT
#                    Ticker: ADANIPOWER.NS (NSE India)
# =============================================================================
# This project covers:
#   Part 01 : Financial Data Acquisition & Handling (CSV, Excel, API, Cleaning)
#   Data Visualisation : Line charts, Bar plots, Financial charts (ggplot2)
#   Basic Time Series Analysis : ARIMA, Trends, Seasonal Decomposition
#   Part 02 : Algorithmic Trading
# =============================================================================
# ─────────────────────────────────────────────────────────────────────────────
# SECTION 0 : INSTALL & LOAD REQUIRED PACKAGES
# ─────────────────────────────────────────────────────────────────────────────
required_packages <- c(
  "quantmod",       # Yahoo Finance API data download
  "tidyverse",      # dplyr, ggplot2, tidyr, readr, etc.
  "ggplot2",        # Data visualisation
  "lubridate",      # Date handling
  "xts",            # Extensible time-series
  "zoo",            # Time-series infrastructure
  "TTR",            # Technical Trading Rules (SMA, EMA, RSI, MACD)
  "tseries",        # ADF test, GARCH
  "forecast",       # ARIMA, auto.arima, decomposition
  "writexl",        # Write Excel files
  "readxl",         # Read Excel files
  "PerformanceAnalytics", # Portfolio analytics
  "scales",         # Axis formatting
  "gridExtra"       # Arrange multiple plots
)
# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
}
# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))
cat("\n✅ All packages loaded successfully!\n\n")
# =============================================================================
#          PART 01 : FINANCIAL DATA ACQUISITION & HANDLING
# =============================================================================
# ─────────────────────────────────────────────────────────────────────────────
# 1.1  Download Data from Yahoo Finance API using quantmod
# ─────────────────────────────────────────────────────────────────────────────
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PART 01 : FINANCIAL DATA ACQUISITION & HANDLING\n")
cat("═══════════════════════════════════════════════════════════════\n\n")
ticker <- "ADANIPOWER.NS"
start_date <- "2020-01-01"
end_date <- Sys.Date()
cat(paste("📥 Downloading data for:", ticker, "\n"))
cat(paste("   Period:", start_date, "to", end_date, "\n\n"))
# Download OHLCV data from Yahoo Finance
getSymbols(
  Symbols = ticker,
  src     = "yahoo",
  from    = start_date,
  to      = end_date,
  auto.assign = TRUE
)
# Extract the xts object
adani_xts <- get(ticker)
cat(paste("✅ Downloaded", nrow(adani_xts), "rows of data for", ticker, "\n\n"))
# Preview the raw data
cat("── First 6 rows of raw data ──\n")
print(head(adani_xts))
cat("\n── Last 6 rows of raw data ──\n")
print(tail(adani_xts))
cat("\n")
# ─────────────────────────────────────────────────────────────────────────────
# 1.2  Convert xts to a Clean Data Frame
# ─────────────────────────────────────────────────────────────────────────────
adani_df <- data.frame(
  Date     = index(adani_xts),
  Open     = as.numeric(Op(adani_xts)),
  High     = as.numeric(Hi(adani_xts)),
  Low      = as.numeric(Lo(adani_xts)),
  Close    = as.numeric(Cl(adani_xts)),
  Volume   = as.numeric(Vo(adani_xts)),
  Adjusted = as.numeric(Ad(adani_xts)),
  stringsAsFactors = FALSE
)
# Remove the auto-generated row names
rownames(adani_df) <- NULL
cat("── Structure of the Data Frame ──\n")
str(adani_df)
cat("\n")
# ─────────────────────────────────────────────────────────────────────────────
# 1.3  Data Cleaning & Preprocessing
# ─────────────────────────────────────────────────────────────────────────────
cat("── Data Cleaning ──\n\n")
# 1.3.1 Check for missing values
missing_summary <- colSums(is.na(adani_df))
cat("Missing values per column:\n")
print(missing_summary)
cat("\n")
# 1.3.2 Handle missing values — forward fill then backward fill
adani_df <- adani_df %>%
  arrange(Date) %>%
  tidyr::fill(Open, High, Low, Close, Volume, Adjusted, .direction = "downup")
cat("Missing values AFTER cleaning:\n")
print(colSums(is.na(adani_df)))
cat("\n")
# 1.3.3 Remove duplicate dates (if any)
n_before <- nrow(adani_df)
adani_df <- adani_df %>% distinct(Date, .keep_all = TRUE)
n_after <- nrow(adani_df)
cat(paste("Duplicates removed:", n_before - n_after, "\n\n"))
# 1.3.4 Add derived columns
adani_df <- adani_df %>%
  mutate(
    Daily_Return   = (Close - lag(Close)) / lag(Close) * 100,
    Log_Return     = log(Close / lag(Close)) * 100,
    Price_Range    = High - Low,
    Avg_Price      = (Open + High + Low + Close) / 4,
    Year           = year(Date),
    Month          = month(Date, label = TRUE),
    Day            = wday(Date, label = TRUE),
    Quarter        = quarter(Date)
  )
cat("── Added derived columns: Daily_Return, Log_Return, Price_Range, Avg_Price, Year, Month, Day, Quarter ──\n")
# 1.3.5 Summary statistics
cat("\n── Summary Statistics ──\n")
print(summary(adani_df))
cat("\n")
# ─────────────────────────────────────────────────────────────────────────────
# 1.4  Save Data to CSV
# ─────────────────────────────────────────────────────────────────────────────
csv_file <- "Adani_Power_Stock_Data.csv"
write.csv(adani_df, file = csv_file, row.names = FALSE)
cat(paste("💾 Data saved to CSV:", csv_file, "\n"))
# ─────────────────────────────────────────────────────────────────────────────
# 1.5  Save Data to Excel
# ─────────────────────────────────────────────────────────────────────────────
excel_file <- "Adani_Power_Stock_Data.xlsx"
writexl::write_xlsx(adani_df, path = excel_file)
cat(paste("💾 Data saved to Excel:", excel_file, "\n"))
# ─────────────────────────────────────────────────────────────────────────────
# 1.6  Read Data Back from CSV and Excel (Demonstrate Reading)
# ─────────────────────────────────────────────────────────────────────────────
# Read from CSV
adani_csv <- read.csv(csv_file, stringsAsFactors = FALSE)
adani_csv$Date <- as.Date(adani_csv$Date)
cat(paste("📂 Read back from CSV:", nrow(adani_csv), "rows\n"))
# Read from Excel
adani_excel <- readxl::read_excel(excel_file)
adani_excel$Date <- as.Date(adani_excel$Date)
cat(paste("📂 Read back from Excel:", nrow(adani_excel), "rows\n\n"))
cat("✅ PART 01 COMPLETE — Data Acquisition & Handling Done!\n\n")
# =============================================================================
#          DATA VISUALISATION (using ggplot2)
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  DATA VISUALISATION\n")
cat("═══════════════════════════════════════════════════════════════\n\n")
# Set a professional theme for all plots
theme_adani <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16, hjust = 0.5,
                                    color = "#1a1a2e"),
    plot.subtitle    = element_text(hjust = 0.5, color = "#555555"),
    plot.caption     = element_text(color = "#888888", size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )
# ─────────────────────────────────────────────────────────────────────────────
# 2.1  LINE CHART — Closing Price Over Time
# ─────────────────────────────────────────────────────────────────────────────
p1 <- ggplot(adani_df, aes(x = Date, y = Close)) +
  geom_line(color = "#0d6efd", linewidth = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "#e63946",
              fill = "#ffc3c3", alpha = 0.3, linewidth = 0.8) +
  labs(
    title    = "Adani Power — Closing Price Trend",
    subtitle = paste(start_date, "to", end_date),
    x = "Date", y = "Closing Price (INR)",
    caption  = "Source: Yahoo Finance | ADANIPOWER.NS"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(labels = scales::comma) +
  theme_adani
print(p1)
cat("📊 Plot 1: Closing Price Line Chart — Done\n")
# ─────────────────────────────────────────────────────────────────────────────
# 2.2  BAR PLOT — Average Monthly Volume
# ─────────────────────────────────────────────────────────────────────────────
monthly_volume <- adani_df %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Avg_Volume = mean(Volume, na.rm = TRUE), .groups = "drop")
p2 <- ggplot(monthly_volume, aes(x = YearMonth, y = Avg_Volume)) +
  geom_col(fill = "#2d6a4f", alpha = 0.85, width = 25) +
  labs(
    title    = "Adani Power — Average Monthly Trading Volume",
    subtitle = "Volume traded per month",
    x = "Month", y = "Average Volume",
    caption  = "Source: Yahoo Finance | ADANIPOWER.NS"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(labels = scales::comma) +
  theme_adani
print(p2)
cat("📊 Plot 2: Monthly Volume Bar Plot — Done\n")
# ─────────────────────────────────────────────────────────────────────────────
# 2.3  BAR PLOT — Average Closing Price by Year
# ─────────────────────────────────────────────────────────────────────────────
yearly_avg <- adani_df %>%
  group_by(Year) %>%
  summarise(Avg_Close = mean(Close, na.rm = TRUE), .groups = "drop")
p3 <- ggplot(yearly_avg, aes(x = factor(Year), y = Avg_Close, fill = factor(Year))) +
  geom_col(show.legend = FALSE, alpha = 0.9, width = 0.6) +
  geom_text(aes(label = round(Avg_Close, 1)), vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title   = "Adani Power — Average Closing Price by Year",
    x = "Year", y = "Average Close (INR)",
    caption = "Source: Yahoo Finance"
  ) +
  theme_adani
print(p3)
cat("📊 Plot 3: Yearly Avg Close Bar Chart — Done\n")
# ─────────────────────────────────────────────────────────────────────────────
# 2.4  CANDLESTICK-STYLE CHART (High-Low-Close) — Last 90 Trading Days
# ─────────────────────────────────────────────────────────────────────────────
recent_90 <- tail(adani_df, 90)
p4 <- ggplot(recent_90, aes(x = Date)) +
  geom_linerange(aes(ymin = Low, ymax = High), color = "#6c757d", linewidth = 0.4) +
  geom_point(aes(y = Close, color = ifelse(Close >= Open, "Up", "Down")), size = 1.8) +
  scale_color_manual(values = c("Up" = "#2d6a4f", "Down" = "#e63946"), name = "Direction") +
  labs(
    title    = "Adani Power — High-Low-Close Chart (Last 90 Days)",
    subtitle = "Green = Close >= Open | Red = Close < Open",
    x = "Date", y = "Price (INR)",
    caption  = "Source: Yahoo Finance | ADANIPOWER.NS"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_adani
print(p4)
cat("📊 Plot 4: High-Low-Close Financial Chart — Done\n")
# ─────────────────────────────────────────────────────────────────────────────
# 2.5  HISTOGRAM — Distribution of Daily Returns
# ─────────────────────────────────────────────────────────────────────────────
p5 <- ggplot(adani_df %>% filter(!is.na(Daily_Return)), aes(x = Daily_Return)) +
  geom_histogram(bins = 60, fill = "#457b9d", color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title    = "Adani Power — Distribution of Daily Returns (%)",
    x = "Daily Return (%)", y = "Frequency",
    caption  = "Source: Yahoo Finance"
  ) +
  theme_adani
print(p5)
cat("📊 Plot 5: Daily Returns Histogram — Done\n")
# ─────────────────────────────────────────────────────────────────────────────
# 2.6  MOVING AVERAGES PLOT (SMA 20, SMA 50, SMA 200)
# ─────────────────────────────────────────────────────────────────────────────
adani_df <- adani_df %>%
  arrange(Date) %>%
  mutate(
    SMA_20  = TTR::SMA(Close, n = 20),
    SMA_50  = TTR::SMA(Close, n = 50),
    SMA_200 = TTR::SMA(Close, n = 200)
  )
p6 <- ggplot(adani_df, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), linewidth = 0.5) +
  geom_line(aes(y = SMA_20, color = "SMA 20"), linewidth = 0.6, na.rm = TRUE) +
  geom_line(aes(y = SMA_50, color = "SMA 50"), linewidth = 0.6, na.rm = TRUE) +
  geom_line(aes(y = SMA_200, color = "SMA 200"), linewidth = 0.7, na.rm = TRUE) +
  scale_color_manual(
    values = c("Close" = "#1a1a2e", "SMA 20" = "#e63946",
               "SMA 50" = "#f4a261", "SMA 200" = "#2a9d8f"),
    name = "Series"
  ) +
  labs(
    title    = "Adani Power — Price with Moving Averages",
    subtitle = "SMA 20 (Red) | SMA 50 (Orange) | SMA 200 (Teal)",
    x = "Date", y = "Price (INR)",
    caption  = "Source: Yahoo Finance | ADANIPOWER.NS"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_adani
print(p6)
cat("📊 Plot 6: Moving Averages Chart — Done\n")
# ─────────────────────────────────────────────────────────────────────────────
# 2.7  BOXPLOT — Daily Returns by Year
# ─────────────────────────────────────────────────────────────────────────────
p7 <- ggplot(adani_df %>% filter(!is.na(Daily_Return)),
             aes(x = factor(Year), y = Daily_Return, fill = factor(Year))) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title   = "Adani Power — Daily Returns Distribution by Year",
    x = "Year", y = "Daily Return (%)",
    caption = "Source: Yahoo Finance"
  ) +
  theme_adani
print(p7)
cat("📊 Plot 7: Boxplot of Returns by Year — Done\n\n")
cat("✅ DATA VISUALISATION COMPLETE!\n\n")
# =============================================================================
#          BASIC TIME SERIES ANALYSIS
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  BASIC TIME SERIES ANALYSIS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 3.1  Create Time Series Object
# ─────────────────────────────────────────────────────────────────────────────
# Use monthly average close price for time-series decomposition
monthly_ts_df <- adani_df %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Avg_Close = mean(Close, na.rm = TRUE), .groups = "drop") %>%
  arrange(YearMonth)
# Determine start year and month
ts_start_year  <- year(min(monthly_ts_df$YearMonth))
ts_start_month <- month(min(monthly_ts_df$YearMonth))
# Create ts object (monthly frequency = 12)
adani_ts <- ts(
  monthly_ts_df$Avg_Close,
  start     = c(ts_start_year, ts_start_month),
  frequency = 12
)
cat("── Time Series Object ──\n")
print(adani_ts)
cat("\n")
# ─────────────────────────────────────────────────────────────────────────────
# 3.2  Trend Analysis
# ─────────────────────────────────────────────────────────────────────────────
cat("── Trend Analysis ──\n")
# Simple linear trend on monthly data
trend_data <- data.frame(
  Time      = 1:length(adani_ts),
  Avg_Close = as.numeric(adani_ts)
)
trend_model <- lm(Avg_Close ~ Time, data = trend_data)
cat("Linear Trend Model Summary:\n")
print(summary(trend_model))
# Plot trend
p_trend <- ggplot(trend_data, aes(x = Time, y = Avg_Close)) +
  geom_line(color = "#0d6efd", linewidth = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#e63946",
              fill = "#ffc3c3", alpha = 0.3) +
  labs(
    title    = "Adani Power — Monthly Avg Close with Linear Trend",
    x = "Time (Months since start)", y = "Average Close (INR)",
    caption  = "Linear regression trend line in red"
  ) +
  theme_adani
print(p_trend)
cat("📊 Trend Plot — Done\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 3.3  Seasonal Decomposition (STL)
# ─────────────────────────────────────────────────────────────────────────────
cat("── Seasonal Decomposition ──\n")
# STL decomposition (requires at least 2 full cycles, i.e., 24 months)
if (length(adani_ts) >= 24) {
  decomp <- stl(adani_ts, s.window = "periodic")
  cat("STL Decomposition Components:\n")
  print(summary(decomp))
  
  # Plot decomposition
  plot(decomp, main = "Adani Power — STL Decomposition (Monthly Avg Close)")
  cat("📊 STL Decomposition Plot — Done\n\n")
  
  # Also plot using ggplot for nicer visuals
  decomp_df <- data.frame(
    Date      = monthly_ts_df$YearMonth,
    Observed  = as.numeric(adani_ts),
    Trend     = as.numeric(decomp$time.series[, "trend"]),
    Seasonal  = as.numeric(decomp$time.series[, "seasonal"]),
    Remainder = as.numeric(decomp$time.series[, "remainder"])
  )
  
  decomp_long <- decomp_df %>%
    pivot_longer(cols = c(Observed, Trend, Seasonal, Remainder),
                 names_to = "Component", values_to = "Value") %>%
    mutate(Component = factor(Component,
                              levels = c("Observed", "Trend", "Seasonal", "Remainder")))
  
  p_decomp <- ggplot(decomp_long, aes(x = Date, y = Value)) +
    geom_line(color = "#2a9d8f", linewidth = 0.6) +
    facet_wrap(~ Component, scales = "free_y", ncol = 1) +
    labs(
      title   = "Adani Power — Time Series Decomposition (ggplot2)",
      x = "Date", y = "Value",
      caption = "STL decomposition: Trend, Seasonal, Remainder"
    ) +
    theme_adani +
    theme(strip.text = element_text(face = "bold", size = 12))
  
  print(p_decomp)
  cat("📊 ggplot2 Decomposition Plot — Done\n\n")
} else {
  cat("⚠️  Not enough data for seasonal decomposition (need >= 24 months).\n\n")
}
# ─────────────────────────────────────────────────────────────────────────────
# 3.4  Stationarity Test (Augmented Dickey-Fuller)
# ─────────────────────────────────────────────────────────────────────────────
cat("── Stationarity Test (ADF) ──\n")
adf_result <- adf.test(adani_ts, alternative = "stationary")
cat(paste("ADF Test Statistic:", round(adf_result$statistic, 4), "\n"))
cat(paste("P-value:           ", round(adf_result$p.value, 4), "\n"))
if (adf_result$p.value < 0.05) {
  cat("➡️  Result: The series IS stationary (reject H0) at 5% significance.\n\n")
} else {
  cat("➡️  Result: The series is NOT stationary (fail to reject H0). Differencing needed.\n\n")
}
# ─────────────────────────────────────────────────────────────────────────────
# 3.5  ARIMA Model — Auto Selection
# ─────────────────────────────────────────────────────────────────────────────
cat("── ARIMA Modeling ──\n")
# auto.arima selects the best (p,d,q)(P,D,Q)[m] model automatically
arima_model <- auto.arima(adani_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
cat("Best ARIMA Model:\n")
print(summary(arima_model))
cat("\n")
# Check residuals
checkresiduals(arima_model)
cat("📊 ARIMA Residual Diagnostics — Done\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 3.6  ARIMA Forecast — Next 6 Months
# ─────────────────────────────────────────────────────────────────────────────
cat("── ARIMA Forecast (6 Months Ahead) ──\n")
forecast_horizon <- 6
arima_forecast <- forecast(arima_model, h = forecast_horizon)
cat("Forecasted Values:\n")
print(arima_forecast)
cat("\n")
# Plot forecast
p_forecast <- autoplot(arima_forecast) +
  labs(
    title    = paste("Adani Power — ARIMA Forecast (Next", forecast_horizon, "Months)"),
    subtitle = paste("Model:", arima_model$arma[1], ",", arima_model$arma[6], ",", arima_model$arma[2]),
    x = "Time", y = "Monthly Avg Close (INR)",
    caption  = "80% and 95% confidence intervals shown"
  ) +
  theme_adani
print(p_forecast)
cat("📊 ARIMA Forecast Plot — Done\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 3.7  ACF and PACF Plots
# ─────────────────────────────────────────────────────────────────────────────
cat("── ACF & PACF Plots ──\n")
par(mfrow = c(1, 2))
acf(adani_ts, main = "Adani Power — ACF (Monthly Avg Close)", lag.max = 36)
pacf(adani_ts, main = "Adani Power — PACF (Monthly Avg Close)", lag.max = 36)
par(mfrow = c(1, 1))
cat("📊 ACF & PACF Plots — Done\n\n")
cat("✅ BASIC TIME SERIES ANALYSIS COMPLETE!\n\n")
# =============================================================================
#          PART 02 : ALGORITHMIC TRADING
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  PART 02 : ALGORITHMIC TRADING\n")
cat("═══════════════════════════════════════════════════════════════\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 4.1  Calculate Technical Indicators
# ─────────────────────────────────────────────────────────────────────────────
cat("── Calculating Technical Indicators ──\n")
adani_df <- adani_df %>%
  arrange(Date) %>%
  mutate(
    # Exponential Moving Averages
    EMA_12  = TTR::EMA(Close, n = 12),
    EMA_26  = TTR::EMA(Close, n = 26),
    
    # Bollinger Bands (20-day, 2 std dev)
    BB_Mid   = TTR::SMA(Close, n = 20),
    BB_Upper = BB_Mid + 2 * TTR::runSD(Close, n = 20),
    BB_Lower = BB_Mid - 2 * TTR::runSD(Close, n = 20)
  )
# RSI (14-day)
adani_df$RSI_14 <- TTR::RSI(adani_df$Close, n = 14)
# MACD
macd_result <- TTR::MACD(adani_df$Close, nFast = 12, nSlow = 26, nSig = 9)
adani_df$MACD_Line   <- macd_result[, "macd"]
adani_df$MACD_Signal <- macd_result[, "signal"]
adani_df$MACD_Hist   <- adani_df$MACD_Line - adani_df$MACD_Signal
cat("✅ Technical indicators calculated: EMA 12/26, Bollinger Bands, RSI 14, MACD\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 4.2  STRATEGY 1 : SMA Crossover (Golden Cross / Death Cross)
# ─────────────────────────────────────────────────────────────────────────────
cat("── Strategy 1: SMA Crossover (50/200) ──\n")
adani_df <- adani_df %>%
  mutate(
    # Signal: 1 = BUY (SMA50 > SMA200), -1 = SELL (SMA50 < SMA200), 0 = no signal
    SMA_Signal = case_when(
      SMA_50 > SMA_200 & lag(SMA_50) <= lag(SMA_200) ~  1,   # Golden Cross (BUY)
      SMA_50 < SMA_200 & lag(SMA_50) >= lag(SMA_200) ~ -1,   # Death Cross (SELL)
      TRUE ~ 0
    ),
    # Position: 1 = Long, 0 = Out
    SMA_Position = ifelse(SMA_50 > SMA_200, 1, 0)
  )
# Count signals
sma_buy_signals  <- sum(adani_df$SMA_Signal == 1, na.rm = TRUE)
sma_sell_signals <- sum(adani_df$SMA_Signal == -1, na.rm = TRUE)
cat(paste("  Golden Cross (BUY) signals :", sma_buy_signals, "\n"))
cat(paste("  Death Cross (SELL) signals :", sma_sell_signals, "\n"))
# Calculate strategy returns
adani_df <- adani_df %>%
  mutate(
    SMA_Strategy_Return = SMA_Position * Daily_Return,
    SMA_Cum_Return      = cumprod(1 + ifelse(is.na(SMA_Strategy_Return), 0,
                                             SMA_Strategy_Return / 100)) - 1,
    BuyHold_Cum_Return  = cumprod(1 + ifelse(is.na(Daily_Return), 0,
                                             Daily_Return / 100)) - 1
  )
# Plot SMA Crossover signals on price chart
p_sma_strategy <- ggplot(adani_df, aes(x = Date)) +
  geom_line(aes(y = Close), color = "#1a1a2e", linewidth = 0.4, alpha = 0.7) +
  geom_line(aes(y = SMA_50), color = "#e63946", linewidth = 0.5, na.rm = TRUE) +
  geom_line(aes(y = SMA_200), color = "#2a9d8f", linewidth = 0.5, na.rm = TRUE) +
  geom_point(data = adani_df %>% filter(SMA_Signal == 1),
             aes(y = Close), color = "#2d6a4f", size = 3, shape = 24, fill = "#2d6a4f") +
  geom_point(data = adani_df %>% filter(SMA_Signal == -1),
             aes(y = Close), color = "#e63946", size = 3, shape = 25, fill = "#e63946") +
  labs(
    title    = "Adani Power — SMA Crossover Trading Strategy",
    subtitle = "▲ Green = BUY (Golden Cross) | ▼ Red = SELL (Death Cross)",
    x = "Date", y = "Price (INR)",
    caption  = "SMA 50 (Red) vs SMA 200 (Teal)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_adani
print(p_sma_strategy)
cat("📊 SMA Crossover Strategy Plot — Done\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 4.3  STRATEGY 2 : RSI-Based Strategy
# ─────────────────────────────────────────────────────────────────────────────
cat("── Strategy 2: RSI-Based Strategy ──\n")
adani_df <- adani_df %>%
  mutate(
    RSI_Signal = case_when(
      RSI_14 < 30 & lag(RSI_14) >= 30 ~  1,   # Oversold → BUY
      RSI_14 > 70 & lag(RSI_14) <= 70 ~ -1,   # Overbought → SELL
      TRUE ~ 0
    ),
    RSI_Position = case_when(
      RSI_14 < 30 ~ 1,
      RSI_14 > 70 ~ 0,
      TRUE ~ NA_real_
    )
  )
# Forward-fill RSI position
adani_df$RSI_Position <- zoo::na.locf(adani_df$RSI_Position, na.rm = FALSE)
adani_df$RSI_Position[is.na(adani_df$RSI_Position)] <- 0
rsi_buy  <- sum(adani_df$RSI_Signal == 1, na.rm = TRUE)
rsi_sell <- sum(adani_df$RSI_Signal == -1, na.rm = TRUE)
cat(paste("  RSI BUY signals (< 30) :", rsi_buy, "\n"))
cat(paste("  RSI SELL signals (> 70):", rsi_sell, "\n"))
adani_df <- adani_df %>%
  mutate(
    RSI_Strategy_Return = RSI_Position * Daily_Return,
    RSI_Cum_Return      = cumprod(1 + ifelse(is.na(RSI_Strategy_Return), 0,
                                             RSI_Strategy_Return / 100)) - 1
  )
# RSI Plot
p_rsi <- ggplot(adani_df %>% filter(!is.na(RSI_14)), aes(x = Date, y = RSI_14)) +
  geom_line(color = "#6a0dad", linewidth = 0.5) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_hline(yintercept = 30, linetype = "dashed", color = "green", linewidth = 0.6) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "grey50", linewidth = 0.4) +
  geom_ribbon(aes(ymin = 30, ymax = pmin(RSI_14, 30)), fill = "#2d6a4f", alpha = 0.2) +
  geom_ribbon(aes(ymin = pmax(RSI_14, 70), ymax = 70), fill = "#e63946", alpha = 0.2) +
  labs(
    title    = "Adani Power — RSI (14-Day)",
    subtitle = "Overbought > 70 (Red zone) | Oversold < 30 (Green zone)",
    x = "Date", y = "RSI",
    caption  = "Source: Yahoo Finance"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_adani
print(p_rsi)
cat("📊 RSI Strategy Plot — Done\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 4.4  STRATEGY 3 : MACD Crossover Strategy
# ─────────────────────────────────────────────────────────────────────────────
cat("── Strategy 3: MACD Crossover Strategy ──\n")
adani_df <- adani_df %>%
  mutate(
    MACD_Signal_Trade = case_when(
      MACD_Line > MACD_Signal & lag(MACD_Line) <= lag(MACD_Signal) ~  1,  # BUY
      MACD_Line < MACD_Signal & lag(MACD_Line) >= lag(MACD_Signal) ~ -1,  # SELL
      TRUE ~ 0
    ),
    MACD_Position = ifelse(MACD_Line > MACD_Signal, 1, 0)
  )
macd_buy  <- sum(adani_df$MACD_Signal_Trade == 1, na.rm = TRUE)
macd_sell <- sum(adani_df$MACD_Signal_Trade == -1, na.rm = TRUE)
cat(paste("  MACD BUY signals :", macd_buy, "\n"))
cat(paste("  MACD SELL signals:", macd_sell, "\n"))
adani_df <- adani_df %>%
  mutate(
    MACD_Strategy_Return = MACD_Position * Daily_Return,
    MACD_Cum_Return      = cumprod(1 + ifelse(is.na(MACD_Strategy_Return), 0,
                                              MACD_Strategy_Return / 100)) - 1
  )
# MACD Plot
p_macd <- ggplot(adani_df %>% filter(!is.na(MACD_Line)), aes(x = Date)) +
  geom_line(aes(y = MACD_Line, color = "MACD Line"), linewidth = 0.6) +
  geom_line(aes(y = MACD_Signal, color = "Signal Line"), linewidth = 0.6) +
  geom_bar(aes(y = MACD_Hist, fill = ifelse(MACD_Hist >= 0, "Positive", "Negative")),
           stat = "identity", alpha = 0.5, width = 1) +
  scale_color_manual(values = c("MACD Line" = "#0d6efd", "Signal Line" = "#e63946"),
                     name = "Lines") +
  scale_fill_manual(values = c("Positive" = "#2d6a4f", "Negative" = "#e63946"),
                    name = "Histogram") +
  labs(
    title    = "Adani Power — MACD Indicator",
    subtitle = "MACD Line vs Signal Line with Histogram",
    x = "Date", y = "MACD Value",
    caption  = "Fast: 12 | Slow: 26 | Signal: 9"
  ) +
  theme_adani
print(p_macd)
cat("📊 MACD Strategy Plot — Done\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 4.5  Bollinger Bands Plot
# ─────────────────────────────────────────────────────────────────────────────
p_bb <- ggplot(adani_df %>% filter(!is.na(BB_Upper)), aes(x = Date)) +
  geom_ribbon(aes(ymin = BB_Lower, ymax = BB_Upper), fill = "#caf0f8", alpha = 0.5) +
  geom_line(aes(y = Close, color = "Close"), linewidth = 0.5) +
  geom_line(aes(y = BB_Mid, color = "BB Middle"), linewidth = 0.5, linetype = "dashed") +
  geom_line(aes(y = BB_Upper, color = "BB Upper"), linewidth = 0.4) +
  geom_line(aes(y = BB_Lower, color = "BB Lower"), linewidth = 0.4) +
  scale_color_manual(
    values = c("Close" = "#1a1a2e", "BB Middle" = "#e63946",
               "BB Upper" = "#457b9d", "BB Lower" = "#457b9d"),
    name = "Series"
  ) +
  labs(
    title    = "Adani Power — Bollinger Bands (20-Day, 2σ)",
    x = "Date", y = "Price (INR)",
    caption  = "Blue shaded area = Bollinger Band range"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_adani
print(p_bb)
cat("📊 Bollinger Bands Plot — Done\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 4.6  Strategy Performance Comparison
# ─────────────────────────────────────────────────────────────────────────────
cat("── Strategy Performance Comparison ──\n\n")
# Compare cumulative returns
performance_df <- adani_df %>%
  filter(!is.na(BuyHold_Cum_Return)) %>%
  select(Date, BuyHold_Cum_Return, SMA_Cum_Return, RSI_Cum_Return, MACD_Cum_Return) %>%
  pivot_longer(cols = -Date, names_to = "Strategy", values_to = "Cum_Return") %>%
  mutate(Strategy = recode(Strategy,
                           "BuyHold_Cum_Return" = "Buy & Hold",
                           "SMA_Cum_Return"     = "SMA Crossover (50/200)",
                           "RSI_Cum_Return"     = "RSI Strategy",
                           "MACD_Cum_Return"    = "MACD Crossover"
  ))
p_perf <- ggplot(performance_df, aes(x = Date, y = Cum_Return * 100, color = Strategy)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c(
    "Buy & Hold"           = "#1a1a2e",
    "SMA Crossover (50/200)" = "#e63946",
    "RSI Strategy"         = "#2a9d8f",
    "MACD Crossover"       = "#f4a261"
  )) +
  labs(
    title    = "Adani Power — Strategy Performance Comparison",
    subtitle = "Cumulative Returns: Buy & Hold vs Trading Strategies",
    x = "Date", y = "Cumulative Return (%)",
    caption  = "All strategies assume no transaction costs"
  ) +
  theme_adani
print(p_perf)
cat("📊 Strategy Performance Comparison Plot — Done\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 4.7  Performance Metrics Summary Table
# ─────────────────────────────────────────────────────────────────────────────
cat("── Performance Metrics Summary ──\n\n")
calc_metrics <- function(returns, name) {
  r <- returns[!is.na(returns)] / 100  # Convert from % to decimal
  total_return    <- (prod(1 + r) - 1) * 100
  annual_return   <- ((1 + total_return / 100)^(252 / length(r)) - 1) * 100
  daily_vol       <- sd(r) * 100
  annual_vol      <- daily_vol * sqrt(252)
  sharpe          <- ifelse(annual_vol > 0, annual_return / annual_vol, NA)
  max_dd          <- min(cummax(cumprod(1 + r)) - cumprod(1 + r)) /
    max(cummax(cumprod(1 + r))) * -100
  win_rate        <- sum(r > 0) / length(r) * 100
  
  data.frame(
    Strategy          = name,
    Total_Return_Pct  = round(total_return, 2),
    Annual_Return_Pct = round(annual_return, 2),
    Annual_Vol_Pct    = round(annual_vol, 2),
    Sharpe_Ratio      = round(sharpe, 3),
    Max_Drawdown_Pct  = round(max_dd, 2),
    Win_Rate_Pct      = round(win_rate, 2),
    stringsAsFactors   = FALSE
  )
}
metrics <- rbind(
  calc_metrics(adani_df$Daily_Return, "Buy & Hold"),
  calc_metrics(adani_df$SMA_Strategy_Return, "SMA Crossover (50/200)"),
  calc_metrics(adani_df$RSI_Strategy_Return, "RSI Strategy"),
  calc_metrics(adani_df$MACD_Strategy_Return, "MACD Crossover")
)
print(metrics)
cat("\n")
# Save metrics to CSV
write.csv(metrics, "Adani_Power_Strategy_Metrics.csv", row.names = FALSE)
cat("💾 Strategy metrics saved to: Adani_Power_Strategy_Metrics.csv\n\n")
# ─────────────────────────────────────────────────────────────────────────────
# 4.8  Save Final Dataset
# ─────────────────────────────────────────────────────────────────────────────
write.csv(adani_df, "Adani_Power_Complete_Analysis.csv", row.names = FALSE)
cat("💾 Complete dataset saved to: Adani_Power_Complete_Analysis.csv\n\n")
cat("✅ PART 02 COMPLETE — Algorithmic Trading Done!\n\n")
# =============================================================================
#                          PROJECT SUMMARY
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("         ADANI POWER — R PROJECT COMPLETE ✅\n")
cat("═══════════════════════════════════════════════════════════════\n\n")
cat("Company  : Adani Power Limited\n")
cat("Ticker   : ADANIPOWER.NS (NSE India)\n")
cat(paste("Period   :", start_date, "to", end_date, "\n"))
cat(paste("Records  :", nrow(adani_df), "trading days\n\n"))
cat("Parts Completed:\n")
cat("  ✅ Part 01 : Data Acquisition & Handling (API, CSV, Excel, Cleaning)\n")
cat("  ✅ Data Vis : Line charts, Bar plots, Financial charts (ggplot2)\n")
cat("  ✅ TS Analysis : Trend, STL Decomposition, ADF Test, ARIMA, Forecast\n")
cat("  ✅ Part 02 : Algorithmic Trading (SMA, RSI, MACD strategies)\n\n")
cat("Output Files:\n")
cat("  📄 Adani_Power_Stock_Data.csv\n")
cat("  📄 Adani_Power_Stock_Data.xlsx\n")
cat("  📄 Adani_Power_Strategy_Metrics.csv\n")
cat("  📄 Adani_Power_Complete_Analysis.csv\n\n")
cat("═══════════════════════════════════════════════════════════════\n")