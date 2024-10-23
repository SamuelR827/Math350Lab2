# Install necessary packages
install.packages("readxl")
install.packages("ggplot2")

# Load libraries
library(readxl)
library(ggplot2)

# Read the Excel file (adjust the path if needed)
data <- read_excel("LabDataG3.xlsx")

# Fit regression models
# Degudent Group
degudent_model <- lm(dE00_Group_D3 ~ Cut_Group_D3, data = data)

# Zirkonzahn Group
zirkonzahn_model <- lm(dE00_Group_Z3 ~ Cut_Group_Z3, data = data)

# Summary of regression for Degudent Group
summary(degudent_model)

# Summary of regression for Zirkonzahn Group
summary(zirkonzahn_model)

# Plot for Degudent Group
ggplot(data, aes(x = Cut_Group_D3, y = dE00_Group_D3)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Degudent Group: Cut Level vs ΔE00") +
  xlab("Cut Level (Degudent)") +
  ylab("ΔE00 (Degudent)") +
  theme_minimal()

# Plot for Zirkonzahn Group
ggplot(data, aes(x = Cut_Group_Z3, y = dE00_Group_Z3)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Zirkonzahn Group: Cut Level vs ΔE00") +
  xlab("Cut Level (Zirkonzahn)") +
  ylab("ΔE00 (Zirkonzahn)") +
  theme_minimal()
