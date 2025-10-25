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
data(airquality)
str(airquality)

# Remove missing values
airquality <- na.omit(airquality)

# Calculate average Ozone by Month
avg_ozone <- tapply(airquality$Ozone, airquality$Month, mean)

# Create labels
labels <- paste("Month", names(avg_ozone))

# Generate Pie Chart
pie(
  avg_ozone,
  labels = labels,
  main = "Average Ozone Levels by Month",
  col = rainbow(length(avg_ozone)),
  clockwise = TRUE
)

# Add legend
legend(
  "topright",
  legend = labels,
  fill = rainbow(length(avg_ozone)),
  title = "Months"
)

# Bar Chart

# Line Chart

# Stacked Bar Chart

# Histogram

# Dot Chart

# Scatter Chart

# Box Plot
