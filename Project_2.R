# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2) # for melting correlation matrix

# Data ingestion
ingest_data <- function(path = "data/data.csv") {
    if (!file.exists(path)) {
        stop(paste("File not Found:", path))
    }
    read.csv(path, header = TRUE, sep = ",")
}

# Data processing
process_data <- function(df) {
    df <- df %>%
        select(-any_of(c("Color", "Sales_Classification"))) %>%
        mutate(across(where(is.character), as.factor))
    return(df)
}

# Choosing subset for visualization
create_subset <- function(df, size) {
    sample_df <- df[sample(nrow(df), min(size, nrow(df))), ]
    return(sample_df)
}

# --- Visualization Functions ---
plot_histogram <- function(df, col, save_path = NULL) {
    p <- ggplot(df, aes_string(x = col)) +
        geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
        labs(
            title = paste("Histogram of", col),
            x = col,
            y = "Count"
        ) +
        theme_minimal()
    if (!is.null(save_path)) ggsave(filename = save_path, plot = p, width = 8, height = 4)
    return(p)
}

plot_box <- function(df, col, save_path = NULL) {
    p <- ggplot(df, aes_string(y = col)) +
        geom_boxplot(fill = "green", color = "black", alpha = 0.7) +
        labs(title = paste("Box Plot of", col), y = col) +
        theme_minimal()
    if (!is.null(save_path)) ggsave(filename = save_path, plot = p, width = 8, height = 4)
    return(p)
}

plot_bar <- function(df, col, save_path = NULL) {
    p <- ggplot(df, aes_string(x = col)) +
        geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
        labs(title = paste("Bar Chart of", col), x = col, y = "Count") +
        theme_minimal()
    if (!is.null(save_path)) ggsave(filename = save_path, plot = p, width = 8, height = 4)
    return(p)
}

plot_scatter <- function(df, x_col, save_path = NULL) {
    p <- ggplot(df, aes_string(x = x_col, y = "Price_USD")) +
        geom_point(alpha = 0.7, color = "darkblue") +
        labs(title = paste("Scatter Plot:", x_col, "vs Price_USD")) +
        theme_minimal()
    if (!is.null(save_path)) ggsave(filename = save_path, plot = p, width = 8, height = 4)
    return(p)
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

# --- Master Function for Plotting ---
plot_features <- function(df) {
    plot_map <- list(
        numeric = list(plot_histogram, plot_box, plot_scatter),
        categorical = list(plot_bar)
    )

    for (col in names(df)) {
        col_type <- if (is.numeric(df[[col]])) "numeric" else "categorical"
        for (plot_fn in plot_map[[col_type]]) {
            print(plot_fn(df, col))
        }
    }

    # Add correlation heatmap at the end
    print(plot_correlation_matrix(df))
}

# Save processed data
write_data_to_csv <- function(df, output_path = "data/processed_data.csv") {
    write.csv(df, output_path, row.names = FALSE)
}

# --- Driver Function ---
main <- function(raw_path = "data/data.csv",
                 subset_size = 100,
                 save_processed = FALSE,
                 processed_path = "data/processed_data.csv") {
    message("=== Loading and Processing Data ===")
    df <- ingest_data(raw_path)
    message(paste("Raw data loaded:", nrow(df), "rows"))

    df <- process_data(df)
    message("Processing complete")

    if (save_processed) write_data_to_csv(df, processed_path)

    message("\n=== Creating Visualizations ===")
    sample_df <- create_subset(df, subset_size)
    plot_features(sample_df)
    message("==== Complete ====\n")
}

if (!interactive()) {
    main()
}
