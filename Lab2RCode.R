
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

# Plot diagnostic plots for Degudent Group
par(mfrow = c(2, 2)) # Arrange 2x2 plot layout for diagnostics
plot(degudent_model, which = 1:4) # Residuals vs Fitted, QQ, Scale-Location, Residuals vs Leverage

# Plot diagnostic plots for Zirkonzahn Group
par(mfrow = c(2, 2)) # Arrange 2x2 plot layout for diagnostics
plot(zirkonzahn_model, which = 1:4) # Residuals vs Fitted, QQ, Scale-Location, Residuals vs Leverage


# Coefficients for Degudent Group
degudent_coeff <- coef(degudent_model)
degudent_cut_estimate <- (1.8 - degudent_coeff[1]) / degudent_coeff[2]

# Coefficients for Zirkonzahn Group
zirkonzahn_coeff <- coef(zirkonzahn_model)
zirkonzahn_cut_estimate <- (1.8 - zirkonzahn_coeff[1]) / zirkonzahn_coeff[2]

# Display estimates
degudent_cut_estimate
zirkonzahn_cut_estimate

