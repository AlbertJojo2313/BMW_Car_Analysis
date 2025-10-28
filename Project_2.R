# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Data ingestion
ingest_data <- function(path = "data/data.csv") {
    if (!file.exists(path)) {
        stop(paste("File not Found:", path))
    }
    read.csv(path, header = TRUE, sep = ",")
}
# Data processing
process_data <- function(df) {
    df <- df %>% select(-any_of(c("Color", "Sales_Classification")))
}
### Choosing subset for visualization
create_subset <- function(df, size = 1000) {
    sample_df <- df[sample(nrow(df), min(size, nrow(df))), ]
}

plot_histogram <- function(df, col, save_path = NULL) {
    p <- ggplot(df, aes_string(x = col)) +
        geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
        labs(
            title = paste("Histogram of ", col),
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
        labs(title = paste("Box Plot of ", col), y = col) +
        theme_minimal()
    if (!is.null(save_path)) ggsave(filename = save_path, plot = p, width = 8, height = 4)
    return(p)
}
plot_bar <- function(df, col, save_path = NULL) {}
# Master function for plotting
plot_features <- function(df) {
    for (col in names(df)) {
        if (is.numeric(df[[col]])) {
            print(plot_histogram(df, col))
            print(plot_box(df, col))
        } else if (is.factor(df[[col]]) || is.character(df[[col]])) {}
    }
}

# Save processed data
write_data_to_csv <- function(df, output_path = "data/processed_data.csv") {
    write.csv(df, output_path, row.names = FALSE)
}

## Driver function
main <- function(run_processing = FALSE,
                 raw_path = "data/data.csv",
                 processed_path = "data/processed_data.csv",
                 subset_size = 1000) {
    if (run_processing) {
        df <- ingest_data(raw_path) %>% process_data()
        write_data_to_csv(df, processed_path)
    } else {
        df <- ingest_data("data/processed_data.csv")
        sample_df <- create_subset(df)
        plot_features(sample_df)
    }
}
if (sys.nframe() == 0) {
    main(run_processing = FALSE)
}
