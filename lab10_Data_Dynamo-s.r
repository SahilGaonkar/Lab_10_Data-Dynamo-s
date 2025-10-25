# ====================================================================
# lab10_Data-Dynamo-s.R
# Collaborative R Visualization Project
# Dataset: airquality 
# Team Members:
# Esha Kambli 2301
# Deversh Shetgaonkar 2302
# Sahil Gaonkar 2305
# Prabhanjan Halvegar 2306
# Atharv Gawas 2313
# Aarchi Teli 2318 
# Sarvadhnya Patil 2321
# Harsh Palyekar 2329
# ====================================================================

# Load dataset
data(airquality)

# View structure
str(airquality)



#Pie Chart 

# Bar Chart
# ------------------------------
# BAR PLOT — by Sarvadhnya (2321)
# ------------------------------

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


# Line Chart

# Stacked Bar Chart

# Histogram

# Dot Chart

# Scatter Chart

# Box Plot
