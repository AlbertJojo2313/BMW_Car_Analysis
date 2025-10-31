#
#--- This script is responsible for plotting
#

# --- Histogram Plot Function ---
plot_histograms <- function(stocks, save_path = NULL, display = FALSE) {
    for (symbol in names(stocks)) {
        df <- stocks[[symbol]]
        plot_list <- list()
        numeric_cols <- names(df)[sapply(df, is.numeric)]

        for (col_name in numeric_cols) {
            p <- ggplot(df, aes(x = .data[[col_name]])) +
                geom_histogram(fill = "blue", color = "black", alpha = 0.7, bins = 30) +
                geom_vline(aes(xintercept = mean(.data[[col_name]], na.rm = TRUE)),
                    color = "green", linetype = "dashed", linewidth = 1
                ) +
                geom_vline(aes(xintercept = median(.data[[col_name]], na.rm = TRUE)),
                    color = "red", linetype = "dashed", linewidth = 1
                ) +
                labs(
                    title = paste("Histogram of", col_name, "in", symbol),
                    x = col_name,
                    y = "Frequency"
                ) +
                theme_minimal()

            if (display) {
                print(p)
            }


            if (!is.null(save_path)) {
                subfolder <- file.path(save_path, symbol)
                if (!dir.exists(subfolder)) {
                    dir.create(subfolder, recursive = TRUE)
                }

                file_name <- file.path(subfolder, paste0(symbol, "_", col_name, "_hist.png"))
                ggsave(file_name, plot = p, width = 6, height = 4)
            }
        }
    }
    invisible(NULL)
}
#--- Box Plots
plot_box_plts <- function(stocks, save_path = NULL, display = FALSE) {
    for (symbol in names(stocks)) {
        df <- stocks[[symbol]]
        plot_list <- list()
        numeric_cols <- names(df)[sapply(df, is.numeric)]

        for (col_name in numeric_cols) {
            # Calc stats
            col_data <- df[[col_name]]
            q1 <- quantile(col_data, 0.25, na.rm = TRUE)
            q2 <- median(col_data, na.rm = TRUE)
            q3 <- quantile(col_data, 0.75, na.rm = TRUE)
            iqr <- IQR(col_data, na.rm = TRUE)

            # Whisker boundaries
            lower_whisker <- q1 - 1.5 * iqr
            upper_whisker <- q3 + 1.5 * iqr

            # Outliers
            outliers <- col_data[col_data < lower_whisker | col_data > upper_whisker]

            # Plot
            p <- ggplot(df, aes(x = "", y = .data[[col_name]])) +
                geom_boxplot(
                    fill = "skyblue", color = "black",
                    outlier.color = "red", outlier.shape = 16, outlier.size = 3
                ) +
                # Labels positioned ON the box plot
                annotate("text",
                    x = 1.4, y = q1,
                    label = paste("Q1 =", round(q1, 2)),
                    hjust = 0, color = "blue", size = 3.5, fontface = "bold"
                ) +
                annotate("text",
                    x = 1.4, y = q2,
                    label = paste("Median =", round(q2, 2)),
                    hjust = 0, color = "darkgreen", size = 4, fontface = "bold"
                ) +
                annotate("text",
                    x = 1.4, y = q3,
                    label = paste("Q3 =", round(q3, 2)),
                    hjust = 0, color = "blue", size = 3.5, fontface = "bold"
                ) +
                labs(
                    title = paste("Box Plot of", col_name, "in", symbol),
                    x = "",
                    y = col_name
                ) +
                theme_minimal() +
                theme(
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()
                )
            if (display) {
                print(p)
            }

            if (!is.null(save_path)) {
                subfolder <- file.path(save_path, symbol)
                if (!dir.exists(subfolder)) {
                    dir.create(subfolder, recursive = TRUE)
                }
                file_name <- file.path(subfolder, paste0(symbol, "_", col_name, "_boxplt.png"))
                ggsave(file_name, plot = p, width = 7, height = 6)
            }
        }
    }
    invisible(NULL)
}
# --- Trends
plot_time_series <- function(stocks, save_path = NULL, display = FALSE) {
    for (symbol in names(stocks)) {
        df <- stocks[[symbol]]
        plot_list <- list()
        numeric_cols <- names(df)[sapply(df, is.numeric)]

        for (col_name in numeric_cols) {
            p <- ggplot(df, aes(x = Date, y = .data[[col_name]])) +
                geom_line(linewidth = 2, alpha = 0.7, linetype = 2) +
                labs(
                    title = paste("Line Plot of ", col_name, "in", symbol),
                    x = "Date",
                    y = col_name
                ) +
                theme_minimal()

            if (display) {
                print(p)
            }

            if (!is.null(save_path)) {
                sub_folder <- file.path(save_path, symbol)
                if (!dir.exists(sub_folder)) {
                    dir.create(sub_folder, recursive = TRUE)
                }
                file_name <- file.path(sub_folder, paste0(symbol, "_", col_name, "_lineplot.png"))
                ggsave(file_name, plot = p, width = 8, height = 7)
            }
        }
    }
    invisible(NULL)
}
