library("readxl")
library(tidyverse)
library(ggplot2)
library(readr)
library(car)
library(pwr)

df = read_excel("/Users/giovanni-lunetta/POLS2073Q-Spring-2023/problemset1/height_data_spr_2022.xlsx")

# storing the proper columns from the data set in x and y respectively.
x <- df$`mother inches`
y <- df$`student inches`

# Problem 1
# creates a linear model.
model <- lm(y~x, df)
summary(model)

# 1.1. What is the value of b0? 20.5451
# 1.2 What is values of b1? 0.7178
# 1.3. Based on the regression result, what is the predicted height of a student
#      whose mother was six feet tall (72 inches) 72 (exact: 72.2267) (round to 
#      nearest inch)
# 1.4. A student whose mother is 72 inches tall measures 70 inches. What would 
#      be the residual (error) associated with this value, given the existing 
#      regression model results?  (Hint: residual = observed – predicted)    
#      2 (exact: 2.2267) (round to nearest inch)

# Problem 2
# Creates an anova table for the linear model.
anova <- aov(y~x, df)
summary(anova)

# Checking the size of the y-hat values and finding the proper index for y to 
# match the fitted values
fitted(model)
y[2:31]

# Calculating SSE
sse <- sum((fitted(model) - y[2:31])^2)
sse

# Calculate SSR (same as MSS)
ssr <- sum((fitted(model) - mean(y[2:31]))^2)
ssr

# Calculate SST (same as TSS)
sst <- ssr + sse
sst

# Reprint the same linear model.
model <- lm(y~x, df)
summary(model)

# Pull MSE value from ANOVA table and take the square root
MSE <- 17.21
root_MSE <- sqrt(MSE)
root_MSE 

# 1.1 What is the value of the Total Sum of Squares (TSS)? (Round to nearest 1s)
#     569 (exact: 569.4667)
# 1.2. What is the value of the Model Sum of Squares (MSS)? (round to nearest 
#      1s) 88 (exact: 87.52593)
# 1.3. What is the value of R-squared in the table in R?  0.1537
# 1.4. R-squared is the quotient of what two values in the upper left “ANoVA 
#      table”? MSS divided by TSS
# 1.5. What is the value of the “average error” (the Root MSE or residual 
#      standard error)?  4.148494
# 1.6. If you square the RMSE, what number does this resemble in the R output 
#      table? MSE 
# 1.7. The F-statistic value is equal to what two values in the upper left ANoVA
#      table?  87.53 and 17.21.

#Problem 3
#check the leverage
hats <- as.data.frame(hatvalues(model))

#sort observations by leverage, descending
hats[order(-hats['hatvalues(model)']), ]

#plot leverage values for each observation
plot(hatvalues(model), type = 'h')

#store residuals
res <- resid(model)

#produce residual vs. fitted plot
plot(fitted(model), res)

#add a horizontal line at 0 
abline(0,0)

# Are there any outliers in the data? (yes or no) Hint: look at the data, not 
# just the scatterplot! No

# Problem 4
# finding critical value at alpha = 0.05 (30 data points - 2 df yields 28 df)
qt(1-0.05/2, 28)

# finding critical value at alpha = 0.01 (30 data points - 2 df yields 28 df)
qt(1-0.01/2, 28)


# Based on our dataset and model, what is the null hypothesis we are testing?
# b. That mothers’ heights have no effect on their childrens’ heights

# What is the critical value of a t-statistic that we need to reject the null 
# hypothesis at a (alpha)=.05 (two-tailed)? Round to .01 (Hint: the way to do it
# in Stata (or R) is in the Computing corner in Ch. 4)  2.0 (exact: 2.048407)

# What is the critical values of a t-statistic that we need to reject the null 
# hypothesis at a(alpha)=.01 (one-tailed)? Round to .01 (Hint: see above)	2.8
# (exact: 2.763262)

# Problem 5
load("/Users/giovanni-lunetta/POLS2073Q-Spring-2023/problemset1/Ch4_Exercise3_Presidents_and_Economy.RData")

# creates a linear model and anova table for model a
model2a <- lm(ChangeGDPpc ~ LagDemPresident, data = dta)
anova2a <- aov(ChangeGDPpc ~ LagDemPresident, data = dta)
summary(model2a)
summary(anova2a)

# creates a linear model and anova table for model b
anova2b <- aov(Unemployment ~ LagDemPresident, data = dta)
model2b <- lm(Unemployment ~ LagDemPresident, data = dta)
summary(model2b)
summary(anova2b)

# plots GDP model
dta %>% 
  ggplot(aes(x=LagDemPresident, y=ChangeGDPpc))+
  geom_point()+
  geom_smooth(method="lm")

# plots Unemployment model
dta %>% 
  ggplot(aes(x=Unemployment, y=ChangeGDPpc))+
  geom_point()+
  geom_smooth(method="lm")

#1. Based on the results, the model estimates suggest that:  
#   a. Democratic Presidents deliver better economic outcomes than Republican Presidents.
# *****IMPORTANT TO NOTE THAT CHANGEGDP IS NOT SIGNIFICANT ENOUGH BUT UNEMPLOYMENT IS*****

# critical value for power curve for GDP
qt(1-0.05, 50)

# creates a power curve
BetaRange = seq(0, 7, 0.01)
stderrorBeta = 164.0
PowerCurve = pnorm(BetaRange/stderrorBeta - 1.675905)
plot(BetaRange, PowerCurve, xlab="Beta",
     ylab="Probability reject null", type="l")

# created a new data set to add data to
dta2 <- dta

# create a new dataframe with the new data
new_data <- data.frame(Year = c(2014:2022), 
                       ChangeGDPpc = c(845, 1087, 530, 915, 1400, 1091, -2245, 3402, 1237), 
                       Unemployment = c(6.2, 5.3, 4.9, 4.4, 3.9, 3.7, 8.1, 5.4, 3.6), 
                       LagDemPresident = c(1, 1, 1, 1, 0, 0, 0, 0, 1))

#**************************************************************************
#**************************************************************************
#**************************************************************************
# bind the new data to the existing dataframe
dta2 <- rbind(dta2, new_data)

# creates a linear model and anova table for model a
model3a <- lm(ChangeGDPpc ~ LagDemPresident, data = dta2)
anova3a <- aov(ChangeGDPpc ~ LagDemPresident, data = dta2)
summary(model3a)
summary(anova3a)

# creates a linear model and anova table for model b
anova3b <- aov(Unemployment ~ LagDemPresident, data = dta2)
model3b <- lm(Unemployment ~ LagDemPresident, data = dta2)
summary(model3b)
summary(anova3b)

# create a new variable z to determine if change is significant
z <- (220-204.4)/164
z

# Are the regression estimates generated with the newer data different or not
# from the original data? If they are different, are they significantly 
# different? No, this is shown from the value of z not being significant and the
# anova table. Neither shows a significant enough change adding new data.
#**************************************************************************
#**************************************************************************
#**************************************************************************

# Problem 6
newheightdata <- read_csv("/Users/giovanni-lunetta/POLS2073Q-Spring-2023/problemset1/newheight23.csv")

# storing the proper columns from the data set in x_new and y_new respectively.
x_new <- newheightdata$`mom_ht`
y_new <- newheightdata$`stud_ht`

# creates a linear model and anova table
model_new <- lm(y_new~x_new, newheightdata)
summary(model_new)

anova_new <- aov(y_new~x_new, newheightdata)
summary(anova_new)

#check the leverage
hats_new <- as.data.frame(hatvalues(model_new))

#sort observations by leverage, descending
hats_new[order(-hats_new['hatvalues(model_new)']), ]

#plot leverage values for each observation
plot(hatvalues(model_new), type = 'h')

#store residuals
res_new <- resid(model_new)

#produce residual vs. fitted plot
plot(fitted(model_new), res_new)

#add a horizontal line at 0 
abline(0,0)

# determine if change is significant
z_new <- (0.7178 - 0.5062) / 0.3183
z_new

# Estimate the bivariate model to see if there is any association between mother
# height and child height in this data.
# There is not significant evidence to suggest that there is an association 
# between mother height and child height in the new data.

# Check to see if there is any heteroskedasticity or any outliers using plots or
# tests refrenced earlier or in the book.
# There are no outliers and no heteroskedasticity.

# Repeat the exercise that you did in #5: Is the new relationship (b1) 
# substantially different from the old one? Use the original height data 
# estimates (b1’s) as the “true” values for the population and variation in b1, 
# that is the true s.e. of b1.  
# No, the change is not substantially different from the old ones. 


