library(readxl)
Lab2DataCleaned <- read_excel("LabDataG3.xlsx")
View(Lab2DataCleaned)

d3model <- lm(Lab2DataCleaned$dE00_Group_D3 ~ Lab2DataCleaned$Cut_Group_D3)
summary(d3model)
plot(x=Lab2DataCleaned$Cut_Group_D3, y=Lab2DataCleaned$dE00_Group_D3)
abline(d3model)

z3model <- lm(Lab2DataCleaned$dE00_Group_Z3 ~ Lab2DataCleaned$Cut_Group_Z3)
summary(z3model)
plot(x=Lab2DataCleaned$Cut_Group_Z3, y=Lab2DataCleaned$dE00_Group_Z3)
abline(z3model)
