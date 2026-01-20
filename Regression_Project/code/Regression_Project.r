# Property of: Justin Campbell
# File Name: Regression_Project.r
# Description: This script provides the code used for the 
# multiple regression project for the purpose of providing 
# recommendations to a university to improve their graduation rate.


# Read in spreadsheet to object

College_Data=read.csv("College.csv")

# Read in predictor variables and response variable to objects

Phd_Data = College_Data$PhD
S_F_Ratio_Data  = College_Data$S.F.Ratio
Out_of_State_Tuition_Data = College_Data$Outstate
Grad_Rate_Data = College_Data$Grad.Rate

# Generate scatterplots for each predictor:

main_phd = "Scatterplot of PhD Count vs Graduation Rate (%)"
main_sfr = "Scatterplot of Student-Faculty Ratio vs Graduation Rate (%)"
main_ost = "Scatterplot of Out-of-State Tuition ($) vs Graduation Rate (%)"
plot(Phd_Data, Grad_Rate_Data, main = paste(strwrap(main_phd, width = 37), collapse = "\n"), xlab = "PhD Count", ylab = "Graduation Rate (%)",pch=20)
plot(S_F_Ratio_Data, Grad_Rate_Data, main = paste(strwrap(main_sfr, width = 37), collapse = "\n"), xlab = "Student Faculty Ratio", ylab = "Graduation Rate (%)",pch=20)
plot(Out_of_State_Tuition_Data, Grad_Rate_Data, main = paste(strwrap(main_ost, width = 37), collapse = "\n"), xlab = "Out of State Tuition ($)", ylab = "Graduation Rate (%)",pch=20)

# Develop multiple regression model:

mr_model=lm(Grad_Rate_Data~Phd_Data+S_F_Ratio_Data+Out_of_State_Tuition_Data)

# Generate summary statistics for multiple regression model:

summary(mr_model)

# Compute the residuals:  

Res_Data = residuals(mr_model)

# Generate residual plot:

main_rp = "Residual Plot of Graduation Rate (%) from Multiple Regression Model"
plot(fitted(mr_model), Res_Data, ylab = "Residuals", xlab = "Predicted", main = paste(strwrap(main_rp, width = 37), collapse = "\n"))

# Generate Normal Probability Plot: 

standard_res = rstandard(mr_model)
qqnorm(standard_res, xlab = "Normal Scores", ylab = "Standardized Residuals",  main = "Normal Probability Plot")
qqline(standard_res)

