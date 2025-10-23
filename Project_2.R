ingest_data <- function(path = "data/data.csv") {
    if (!file.exists(path)) {
        stop(paste("File not Found:", path))
    }
    return(read.csv(path, header = TRUE, sep = ","))
}
process_data <- function(df) {
    drop_cols <- c("Color", "Sales_Classification")
    drop_cols <- drop_cols[drop_cols %in% names(df)]
    df <- subset(df, select = -drop_cols)
    return(df)
}
write_data_to_csv <- function(df, output_path = "data/processed_data.csv") {
    write.csv(df, output_path, row.names = FALSE)
}

## Driver function
main <- function() {
    message("Reading data ...")
    df <- ingest_data()
    message("Processing data ...")
    df <- process_data(df)
    message("Writing Processed data ...")
    write_data_to_csv(df) # Write to CSV
    message("Done! Preview of data")
    head(df, 10)
}
if (sys.nframe() == 0) {
    main()
}
