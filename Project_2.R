library(dplyr)
library(tidyr)
library(ggplot2)

ingest_data <- function(path = "data/data.csv") {
    if (!file.exists(path)) {
        stop(paste("File not Found:", path))
    }
    return(read.csv(path, header = TRUE, sep = ","))
}
process_data <- function(df) {
    drop_cols <- c("Color", "Sales_Classification")
    df <- df %>% select(-any_of(drop_cols))
    return(df)
}
plot_data <- function(df) {
    p <- ggplot(df, aes(x = Engine_Size_L, y = Sales_Volume, colour = model)) +
        geom_point() +
        labs(
            title = "Engine_Size_L vs Sales_Volume",
            x = "Engine Size (L)",
            y = "Sales Volume"
        ) +
        theme_minimal()
    print(p)
}
explore_data <- function(df) {}
write_data_to_csv <- function(df, output_path = "data/processed_data.csv") {
    write.csv(df, output_path, row.names = FALSE)
}

## Driver function
main <- function(run_processing = FALSE) {
    if (run_processing) {
        message("Reading data ...")
        df <- ingest_data(path = "data/data.csv")
        message("Processing data ...")
        df <- process_data(df)
        message("Writing Processed data ...")
        write_data_to_csv(df) # Write to CSV
    } else {
        message("Skipping processing, loading existing processing data...")
        df <- ingest_data("data/processed_data.csv")

        message("\n Plotting Data")
        plot_data(df)
    }
}
if (sys.nframe() == 0) {
    main(run_processing = FALSE)
}
