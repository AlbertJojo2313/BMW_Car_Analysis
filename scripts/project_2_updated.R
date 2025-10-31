library(dplyr)
library(ggplot2)

# --- Data Loading ---
tesla_stock <- read.csv("data/TSLA.csv")
google_stock <- read.csv("data/GoogleStockPrices.csv")
netflix_stock <- read.csv("data/Netflix_Data.csv")
apple_stock <- read.csv("data/apple_5yr_one.csv")

# --- Data Structure ---
stocks <- list(
    TSLA = tesla_stock,
    GOOGL = google_stock,
    NFLX = netflix_stock,
    AAPL = apple_stock
)
# --- Processing Function ---
process_dfs <- function(stocks) {
    drop_cols <- c("X", "Adj.Close", "Adj Close", "Adj_Close")
    for (name in names(stocks)) {
        df <- stocks[[name]]
        df <- df %>% select(-any_of(drop_cols))
        df[["Date"]] <- as.Date(df[["Date"]])
        # Filter by years 2020–2024
        df <- df %>% filter(format(Date, "%Y") >= 2020 & format(Date, "%Y") <= 2024)

        stocks[[name]] <- df
    }
    return(stocks)
}




# --- Run ---
source("scripts/plotting.R")
stocks <- process_dfs(stocks)
plot_histograms(stocks, display = TRUE)
plot_box_plts(stocks, display = TRUE)
plot_time_series(stocks, display  = TRUE)
