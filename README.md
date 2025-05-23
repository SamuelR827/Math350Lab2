# Math350Lab2

## Question 1
Fit a regression model of Y vs X for your two groups for Degudent and Zirkonzahn using Cut Level for
X and ΔE 00 for Y. Provide the scatter plot with the regression curve and a summary of your regression
analysis. Comment on how well your model fits the data.

See the attached pictures for the scatter plot: 

![Scatter Plot Degudent](Pictures/Scatter-Plot-Degudent-Group-600x500.png)
![Scatter Plot Zirkonzahn](Pictures/Scatter-Plot-Zirkonzahn-Group-600x500.png)

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

In our regression analysis for both the Degudent and Zirkonzahn groups, we created a linear regression model with Cut Level as the independent variable and ΔE00 as the dependent variable.

Degudent Group:

    Model Fit: The R-squared is 0.978, meaning 97.8% of the variation in ΔE00 can be explained by Cut Level.
    Coefficients: The intercept is -1.0951, and the slope for Cut Level is 0.0299, indicating a positive relationship between Cut Level and ΔE00.
    Statistical Significance: The p-value for Cut Level is extremely low, confirming a significant fit.

Zirkonzahn Group:

    Model Fit: The R-squared is 0.980, so 98.0% of the variation in ΔE00 is explained by Cut Level.
    Coefficients: The intercept is 0.3253, and the slope is 0.0156, also indicating a positive relationship.
    Statistical Significance: Similar to Degudent, the p-value is very low, showing a significant fit.

Overall, both models fit well, with R-squared values close to 1, showing that Cut Level is a strong predictor of ΔE00 for both groups. The Degudent group has a slightly higher slope, suggesting that ΔE00 increases more steeply with Cut Level compared to Zirkonzahn.



## Question 2
Use the regression lines above to estimate the predicted value of the cut level for the acceptability
threshold of ΔE 00=1.8 μm:
Given the linear regression equation ^y=b0 +b1 x and the specified value yh, we want to estimate the
corresponding level xh that gave rise to it. We obtain a point estimator by solving the estimated
regression equation for x by^xh= yh−b0
b1, where b1 ≠ 0.

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

Results:

    Degudent Cut Estimates Intercept : 90.04147 
    
    Zirkonzahn Cut Estimates Intercept: 98.10153 
    
    Predicted Cut Level for Degudent at ΔE00 = 1.8: 90.04147 
    
    Predicted Cut Level for Zirkonzahn at ΔE00 = 1.8: 98.10153 
  
  

## Question 3
What cut levels should you include in your linear regression to best estimate 
your cut level corresponding to the acceptability threshold of ΔE00=1.8 μm? 
Explain your thinking. *This might be different for Degudent and Zirkonzahn.*

Fit your data with a regression curve, provide the scatter plot with the regression 
line, and a summary analysis. Recalculate the estimated cut level for the acceptability 
threshold using the new regression model. Comment on why you chose this interval 
and the overall fit of your regression line as it pertains to the estimated cut 
level for the acceptability threshold.

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


We found that for Degudent, we needed to omit 300, 400, and 500 from our data because
they were outside of our prediction interval. For Zirkonzahn, we only removed data
at the 500 level, because that gave us the best R-squared.

See the attached pictures for the Adjusted scatter plot: 

![Scatter Plot Degudent Omitted](Pictures/Scatter-Plot-Degudent-Group-Omitted-600x500.png)
![Scatter Plot Zirkonzahn Omitted](Pictures/Scatter-Plot-Zirkonzahn-Group-Omitted-600x500.png)



## Question 4
Check the assumptions for calculating a confidence interval for ^xh, and use a 0.05 significance level to
find the confidence intervals for the predicted values in part (3). To find the confidence interval for^xh,
we take (see main project docs for formula, broke on paste into file)

Compare the two cut levels for Degudent and Zirkonzahn. Is there a statistically significant difference?
Justify your answer.

    # Check assumptions for Degudent model
    par(mfrow = c(2, 2))
    plot(degudent_model_omit)
    
    # Check assumptions for Zirkonzahn model
    plot(zirkonzahn_model_omit)
    par(mfrow = c(1, 1))
    
    # Degudent prediction with confidence interval
    d_new_data <- data.frame(Cut_Group_D3 = 100)
    degudent_prediction <- predict(degudent_model_omit, d_new_data, interval = "confidence", level = 0.95)
    
    # Zirkonzahn prediction with confidence interval
    z_new_data <- data.frame(Cut_Group_Z3 = 100)
    zirkonzahn_prediction <- predict(zirkonzahn_model_omit, z_new_data, interval = "confidence", level = 0.95)
    
    # Display predictions and confidence intervals
    degudent_prediction
    zirkonzahn_prediction

Results:

    Cut level ^xh for Degudent is 100
    
    Confidence Interval for ^xh is (1.9001, 2.068229)
    
    Cut level ^xh for Zirkonzahn is 100
    
    Confidence Interval for ^xh is (1.804379, 1.92392)

The confidence interval has a small overlap, suggesting that the there is no 
significant difference between the two cut levels.
