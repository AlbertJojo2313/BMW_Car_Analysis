library(dplyr)
### Data Processing
tesla_stock <- read.csv("data/TSLA.csv", )
google_stock <- read.csv("data/GoogleStockPrices.csv")
netflix_stock <- read.csv("data/Netflix_Data.csv")
apple_stock <- read.csv("data/apple_5yr_one.csv")

process_df <- function(df) {
    drop_cols <- c("X", "Adj.Close", "Adj Close", "Adj_Close")
    df <- df %>% select(-any_of(drop_cols))

    df[["Date"]] <- as.Date(df[["Date"]])

    return(df)
}

tesla_stock <- process_df(tesla_stock)
netflix_stock <- process_df(netflix_stock)
apple_stock <- process_df(apple_stock)
google_stock <- process_df(google_stock)


create_subset <- function(df, size = 500) {
    sample_df <- df[sample(nrow(df), min(size, nrow(df))), ]
    return(sample_df)
}

# --- New Correlation Matrix + Heatmap Function ---
plot_correlation_matrix <- function(df, save_path = NULL) {
    numeric_df <- df %>% select(where(is.numeric))
    if (ncol(numeric_df) < 2) {
        warning("Not enough numeric columns for correlation matrix.")
        return(NULL)
    }

    corr_matrix <- cor(numeric_df, use = "complete.obs")
    corr_melt <- melt(corr_matrix)

    p <- ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
        labs(
            title = "Correlation Matrix Heatmap",
            x = "",
            y = "",
            fill = "Correlation"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

    if (!is.null(save_path)) ggsave(filename = save_path, plot = p, width = 8, height = 6)
    return(p)
}
print(plot_correlation_matrix(tesla_stock))
print(plot_correlation_matrix(netflix_stock))
print(plot_correlation_matrix(google_stock))
