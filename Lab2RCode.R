
############################################################################
# Question 1
############################################################################

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
summary(degudent_model) # R2 0.977

# Summary of regression for Zirkonzahn Group
summary(zirkonzahn_model) # R2 0.983

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


############################################################################
# Question 2
############################################################################

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

# Define the acceptability threshold for ΔE00
y_h <- 1.8

# Extract coefficients for Degudent Group
degudent_intercept <- coef(degudent_model)[1]
degudent_slope <- coef(degudent_model)[2]
degudent_cut_estimate <- (y_h - degudent_intercept) / degudent_slope

# Extract coefficients for Zirkonzahn Group
zirkonzahn_intercept <- coef(zirkonzahn_model)[1]
zirkonzahn_slope <- coef(zirkonzahn_model)[2]
zirkonzahn_cut_estimate <- (y_h - zirkonzahn_intercept) / zirkonzahn_slope

# Display estimated cut levels
cat("Predicted Cut Level for Degudent at ΔE00 = 1.8:", degudent_cut_estimate, "\n")
cat("Predicted Cut Level for Zirkonzahn at ΔE00 = 1.8:", zirkonzahn_cut_estimate, "\n")


############################################################################
# Question 3
############################################################################

degudent_data_omit <- subset(data, !(`Desired Cut` %in% c(0, 300, 400, 500)))
zirkonzahn_data_omit <- subset(data, !(`Desired Cut` %in% c(0, 500)))

degudent_model_omit <- lm(dE00_Group_D3 ~ Cut_Group_D3, data = degudent_data_omit)
zirkonzahn_model_omit <- lm(dE00_Group_Z3 ~ Cut_Group_Z3, data = zirkonzahn_data_omit)

summary(degudent_model_omit)
summary(zirkonzahn_model_omit)

d_new_data <- data.frame(`Cut_Group_D3` = 500)
z_new_data <- data.frame(`Cut_Group_Z3` = 500)

degudent_prediction <- predict(degudent_model_omit, d_new_data, interval = "prediction", level = 0.95)
zirkonzahn_prediction <- predict(zirkonzahn_model_omit, z_new_data, interval = "prediction", level = 0.95)

degudent_prediction
zirkonzahn_prediction

# Plot for Degudent Group
ggplot(degudent_data_omit, aes(x = Cut_Group_D3, y = dE00_Group_D3)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Degudent Group Omitted: Cut Level vs ΔE00") +
  xlab("Cut Level (Degudent)") +
  ylab("ΔE00 (Degudent)") +
  theme_minimal()

# Plot for Zirkonzahn Group
ggplot(zirkonzahn_data_omit, aes(x = Cut_Group_Z3, y = dE00_Group_Z3)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Zirkonzahn Group Omitted: Cut Level vs ΔE00") +
  xlab("Cut Level (Zirkonzahn)") +
  ylab("ΔE00 (Zirkonzahn)") +
  theme_minimal()


############################################################################
# Question 4
############################################################################

# Check assumptions for Degudent model
par(mfrow = c(2, 2))
plot(degudent_model_omit)

# Check assumptions for Zirkonzahn model
plot(zirkonzahn_model_omit)
par(mfrow = c(1, 1))

# Degudent prediction with confidence interval
d_new_data <- data.frame(Cut_Group_D3 = 500) # Replace 500 with your target value if different
degudent_prediction <- predict(degudent_model_omit, d_new_data, interval = "confidence", level = 0.95)

# Zirkonzahn prediction with confidence interval
z_new_data <- data.frame(Cut_Group_Z3 = 500) # Replace 500 with your target value if different
zirkonzahn_prediction <- predict(zirkonzahn_model_omit, z_new_data, interval = "confidence", level = 0.95)

# Display predictions and confidence intervals
degudent_prediction
zirkonzahn_prediction


