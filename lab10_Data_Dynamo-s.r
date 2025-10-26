# ====================================================================
# lab10_Data_Dynamo-s.R
# Collaborative R Visualization Project
# Dataset: airquality 
# Team Members:
# Esha Kambli 2301 - Pie Chart
# Deversh Shetgaonkar 2302 - Box Plot
# Sahil Gaonkar 2305 - Stacked Bar Chart
# Prabhanjan Halvegar 2306 - Line Chart
# Atharv Gawas 2313 - Dot Chart
# Aarchi Teli 2318 - Histogram
# Sarvadhnya Patil 2321 - Bar Chart
# Harsh Palyekar 2329 - Scatter Chart
# ====================================================================

# ====================================================================
# SECTION 0: SHARED SETUP (DO NOT MODIFY)
# ====================================================================
# Load dataset
data(airquality)

# Shared data preparation
air_data <- na.omit(airquality)
air_data$Month <- factor(air_data$Month,
                         levels = c(5, 6, 7, 8, 9),
                         labels = c("May", "June", "July", "August", "September"))

# Shared color palette
month_colors <- c("steelblue", "seagreen3", "darkorange", "firebrick3", "purple")

# ====================================================================
# SECTION 1: PIE CHART - Esha Kambli (2301)
# ====================================================================
plot_pie_chart <- function() {

  # Calculate average Ozone by Month (using shared air_data)
  avg_ozone <- tapply(air_data$Ozone, air_data$Month, mean)

  # Calculate percentage contribution
  percentages <- round(100 * avg_ozone / sum(avg_ozone), 1)

  # Labels for pie slices and legend
  pie_labels <- paste(names(avg_ozone), "-", percentages, "%")
  legend_labels <- names(avg_ozone)

  # Generate pie chart
  pie(
    avg_ozone,
    labels = pie_labels,
    main = "Average Ozone Levels by Month",
    col = month_colors,
    clockwise = TRUE
  )

  # Add legend
  legend(
    "topright",
    legend = legend_labels,
    fill = month_colors,
    title = "Months"
  )

  
  cat("Pie Chart section - Esha Kambli\n")
}

# ====================================================================
# SECTION 2: BAR CHART - Sarvadhnya Patil (2321)
# ====================================================================
plot_bar_chart <- function() {

# BAR PLOT — by Sarvadhnya (2321)
# Plot: Mean Ozone by Month (airquality dataset)

library(ggplot2)
library(dplyr)

# Load and clean data
data("airquality")
aq <- na.omit(airquality)

ozone_by_month <- aq %>%
  group_by(Month) %>%
  summarise(mean_ozone = mean(Ozone))

ozone_by_month$Month <- factor(ozone_by_month$Month,
                               labels = c("May", "Jun", "Jul", "Aug", "Sep"))

# Create the bar plot
bar_plot <- ggplot(ozone_by_month, aes(x = Month, y = mean_ozone, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Ozone Levels by Month",
       x = "Month",
       y = "Mean Ozone (ppb)",
       caption = "Data Source: airquality dataset") +
  theme_minimal() +
  theme(legend.position = "none")

# Save plot
if (!dir.exists("images")) dir.create("images")
ggsave("images/barplot_ozone_by_month.png", bar_plot, width = 7, height = 5, dpi = 300)

print("Bar plot created and saved to images/barplot_ozone_by_month.png")
  
  cat("Bar Chart section - Sarvadhnya Patil\n")
}


# ====================================================================
# SECTION 3: LINE CHART - Prabhanjan Halvegar (2306)
# ====================================================================
plot_line_chart <- function() {
  
  
  cat("Line Chart section - Prabhanjan Halvegar\n")
}

# ====================================================================
# SECTION 4: STACKED BAR CHART - Sahil Gaonkar (2305)
# ====================================================================
plot_stacked_bar <- function() {
  
  
  cat("Stacked Bar Chart section - Sahil Gaonkar\n")
}

# ====================================================================
# SECTION 5: HISTOGRAM - Aarchi Teli (2318)
# ====================================================================
plot_histogram <- function() {
  # Load required libraries (only if not already loaded)
  if (!require("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required. Please install it with: install.packages('ggplot2')")
  }
  
  # Simple message
  cat("Creating Histogram for Ozone Levels...\n")
  
  # Handle missing values and prepare data
  ozone_data <- na.omit(air_data$Ozone)
  
  # Calculate mean 
  avg_ozone <- mean(ozone_data)
  
  # Create the Histogram using ggplot2
  hist_plot <- ggplot(air_data, aes(x = Ozone)) +
    geom_histogram(
      binwidth = 10,                     # width of bins
      fill = "lightblue", 
      color = "black", 
      alpha = 0.8
    ) +
    geom_vline(aes(xintercept = avg_ozone), 
               color = "red", 
               linetype = "dashed", 
               size = 1.2) +
    labs(
      title = "Distribution of Ozone Levels",
      subtitle = "Histogram showing frequency of Ozone concentrations",
      x = "Ozone (ppb)",
      y = "Frequency",
      caption = "Data Source: airquality dataset"
    ) +
    annotate("text", x = avg_ozone + 5, y = 10, 
             label = paste("Mean =", round(avg_ozone, 1)), 
             color = "red", size = 3.5, hjust = 0) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "gray40")
    )
  
  # Display the plot
  print(hist_plot)
  
  # Save the histogram image 
  if (!dir.exists("images")) dir.create("images")
  ggsave("images/histogram_ozone.png", hist_plot, width = 8, height = 5, dpi = 300)
  
  cat("Histogram section - Aarchi Teli (2318) - Complete\n")
  
  invisible(hist_plot)
}

plot_histogram()


# ====================================================================
# SECTION 6: DOT CHART - Atharv Gawas (2313)
# ====================================================================
plot_dot_chart <- function() {
  # Load required libraries (only if not already loaded)
  if (!require("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required. Please install it with: install.packages('ggplot2')")
  }
  if (!require("dplyr", quietly = TRUE)) {
    stop("dplyr package required. Please install it with: install.packages('dplyr')")
  }
  
  # Prepare data: Get all ozone readings by month
  ozone_data <- air_data %>%
    select(Month, Ozone) %>%
    mutate(Month = factor(Month, levels = c("May", "June", "July", "August", "September")))
  
  # Create the dot plot with multiple points per month
  dot_plot <- ggplot(ozone_data, aes(x = Month, y = Ozone, color = Month)) +
    geom_point(size = 4, alpha = 0.7, position = position_jitter(width = 0.1, seed = 42)) +
    labs(title = "Distribution of Ozone Levels by Month",
         subtitle = "Dot Plot showing individual measurements",
         x = "Month",
         y = "Ozone (ppb)",
         color = "Month",
         caption = "Data Source: airquality dataset") +
    scale_color_manual(values = c("May" = "yellow", 
                                  "June" = "red", 
                                  "July" = "blue", 
                                  "August" = "darkgreen", 
                                  "September" = "pink")) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 10, color = "gray40"),
          legend.position = "right",
          legend.title = element_text(face = "bold"),
          panel.grid.major.x = element_blank())
  
  # Display the plot
  print(dot_plot)
  
  # Save plot (optional - only saves when function is called individually)
  if (!dir.exists("images")) dir.create("images")
  ggsave("images/dotplot_ozone_by_month.png", dot_plot, width = 8, height = 5, dpi = 300)
  
  cat("Dot Chart section - Atharv Gawas (2313) - Complete\n")
  
  # Return the plot object invisibly for potential further use
  invisible(dot_plot)
}

# ====================================================================
# SECTION 7: SCATTER CHART - Harsh Palyekar (2329)
# ====================================================================
plot_scatter_chart <- function() {

  
  cat("Scatter Chart section - Harsh Palyekar\n")
}

# ====================================================================
# SECTION 8: BOX PLOT - Deversh Shetgaonkar (2302)
# ====================================================================
plot_box_plot <- function() {

  
  cat("Box Plot section - Deversh Shetgaonkar\n")
}

# ====================================================================
# MAIN EXECUTION: Run all plots
# ====================================================================
# Set up multi-panel layout for viewing all plots
par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))

# Execute each plot function
plot_pie_chart()
plot_bar_chart()
plot_line_chart()
plot_stacked_bar()
plot_histogram()
plot_dot_chart()
plot_scatter_chart()
plot_box_plot()

# Reset layout
par(mfrow = c(1, 1))
