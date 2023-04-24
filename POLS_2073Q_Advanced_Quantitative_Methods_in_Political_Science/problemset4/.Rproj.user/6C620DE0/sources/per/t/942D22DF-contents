# Load the required packages
library(dplyr)
library(tidyverse)

# QUESTION 1

df <- read_dta("Ch7_Exercise1_Instability.dta")

# ------------------------------------------------------------------------------
# 1(a) The estimated bivariate model shows that there is a negative relationship
# between democracy and instability, with a standardized coefficient for 
# Democracy of -0.045. This means that a one-standard-deviation increase in 
# Democracy is associated with a decrease of approximately 0.045 standard 
# deviations in Instab. The p-value for the coefficient of Democracy is 
# significant at 0.00291, which indicates that we can reject the null hypothesis
# that the coefficient is zero at the 0.05 significance level. The R-squared 
# value is very low at 0.0021, indicating that only a small proportion of the 
# variance in instability can be explained by democracy alone. Regarding 
# endogeneity, we should be cautious about interpreting this relationship as 
# causal due to potential endogeneity concerns. There may be other factors that 
# are driving both democracy and instability, such as economic factors or if 
# there was a war which are not accounted for in this bivariate model. 
# Therefore, we cannot rule out the possibility that the relationship between 
# democracy and instability is spurious or reverse-causal.

model <- lm(scale(instab) ~ scale(democracy), data = df)
summary(model)
coef(model)

# ------------------------------------------------------------------------------
# 1(b) By including lagged GDP as a control variable, the estimated coefficients
# for Democracy and GDPlag have changed. The coefficient for Democracy is now 
# positive and not statistically significant, with an estimated value of 0.026 
# and a p-value of 0.127. This indicates that after controlling for lagged GDP, 
# the relationship between democracy and instability is no longer significant at
# conventional levels. The coefficient for GDPlag is negative and statistically 
# significant, with an estimated value of -0.1525 and a very small p-value 
# (<2e-16). This suggests that lagged GDP is a significant predictor of 
# instability, and that countries with higher GDP in the previous year tend to 
# have lower instability in the current year. The R-squared value has also 
# increased slightly, from 0.0021 in the previous model to 0.0202 in the new 
# model, indicating that the addition of lagged GDP as a control variable has 
# improved the fit of the model. In conclusion, including lagged GDP as a 
# control variable has changed the estimated relationship between democracy and 
# instability. After controlling for GDP, the relationship between democracy and
# instability is no longer significant, suggesting that GDP may be a confounding 
# factor in this relationship. The negative and significant coefficient for 
# lagged GDP suggests that economic growth and development may be an important 
# factor in reducing political instability.

model2 <- lm(scale(instab) ~ scale(democracy) + scale(gdplag), data = df)
summary(model2)

# ------------------------------------------------------------------------------
# 1(c) The estimated coefficient for log(GDPlag) is -0.1656, with a standard 
# error of 0.0181 and a very small p-value (<2e-16). This suggests that there is
# a statistically significant negative relationship between lagged GDP and 
# instability, after controlling for democracy. To interpret the coefficient, we
# can exponentiate it and subtract 1 to obtain the estimated percentage change 
# in GDP associated with a one-unit change in instability. The estimated 
# coefficient suggests that a 1% increase in lagged GDP is associated with a 
# decrease of approximately 0.1526% in instability in the current year, after 
# controlling for democracy. This implies that higher levels of economic growth 
# in the previous year are associated with lower levels of instability in the 
# current year. The R-squared value has increased slightly compared to the 
# previous models, indicating that the inclusion of the log transformation of 
# GDP has improved the fit of the model. The adjusted R-squared value is still 
# low, however, suggesting that there may be other important factors that are 
# driving instability that are not included in the model. In conclusion, the 
# estimated coefficient for log(GDPlag) suggests that there is a negative 
# relationship between lagged GDP and instability, after controlling for 
# democracy. This provides support for the idea that economic growth and 
# development may be an important factor in reducing political instability.


model3 <- lm(scale(instab) ~ scale(democracy) + log(gdplag), data = df)
summary(model3)

exp(coef(model3)["log(gdplag)"]) - 1

# ------------------------------------------------------------------------------
# 1(d) The estimated coefficients for the Cold War dummy variable and the 
# log-transformed Cold War dummy variable are not statistically significant, 
# with p-values of 0.1044. This suggests that the Cold War may not have had a 
# significant effect on political instability, after controlling for democracy 
# and lagged GDP. The coefficient for democracy is positive and statistically 
# significant, suggesting that higher levels of democracy in the previous year 
# are associated with higher levels of instability in the current year, after 
# controlling for lagged GDP and the Cold War. The coefficient for lagged GDP is
# negative and statistically significant, suggesting that higher levels of GDP 
# in the previous year are associated with lower levels of instability in the 
# current year, after controlling for democracy and the Cold War. The R-squared 
# value and the adjusted R-squared value for both models are similar to the 
# previous models, indicating that adding Cold War dummy variables did not 
# substantially improve the fit of the model. Overall, the results suggest that 
# the effect of the Cold War on instability may be mediated by other factors 
# such as economic growth and political regime type, and that adding Cold War 
# dummy variables may not substantially improve the fit of the model.

model4 <- lm(scale(instab) ~ scale(democracy) + log(gdplag) + coldwar, data = df)
summary(model4)

df$logcoldwar <- log(df$coldwar + 1)
model5 <- lm(scale(instab) ~ scale(democracy) + log(gdplag) + logcoldwar, data = df)
summary(model5)

# ------------------------------------------------------------------------------
# 1(e) The output of the quadratic model shows that both the democracy 
# coefficient and democracy squared coefficient are statistically significant, 
# suggesting a non-linear relationship between democracy and political 
# instability. The coefficient on democracy is positive, indicating that 
# political instability tends to increase with democracy, up to a certain point. 
# The coefficient on democracy squared is negative, indicating that political 
# instability tends to decrease with democracy beyond a certain point. The 
# vertex of the quadratic function is at a democracy score of approximately 0.5
# (or 50 on the original scale), which is the point at which the sign on 
# democracy changes. This means that the relationship between democracy and 
# political instability is negative for democracy scores above 50, and positive 
# for democracy scores below 50. The plot shows a U-shaped relationship between 
# democracy and political instability, which supports the idea that the 
# relationship may be non-linear. However, the R-squared value of the model is 
# relatively low, indicating that there may be other factors not accounted for 
# in the model that are also influencing political instability. Overall, the 
# quadratic model and plot provide some evidence for a non-linear relationship 
# between democracy and political instability, and further research could 
# explore this relationship in more depth.

df$democracy_sq <- df$democracy^2
model6 <- lm(scale(instab) ~ scale(democracy) + scale(democracy_sq) + log(gdplag), data = df)
summary(model6)

library(ggplot2)
ggplot(df, aes(x = democracy, y = instab)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  labs(x = "Democracy Score", y = "Instability Index", title = "Relationship between Democracy and Instability (Quadratic Model)")

# Find the vertex of the quadratic function
a <- coef(model6)[["scale(democracy_sq)"]]
b <- coef(model6)[["scale(democracy)"]]
x0 <- -b/(2*a)
y0 <- a*x0^2 + b*x0 + coef(model6)[["(Intercept)"]]
vertex <- data.frame(x0, y0)

# Print the vertex
vertex

# ------------------------------------------------------------------------------
# QUESTION 2
df2 <- read_dta("Ch12_Exercise2_Global_warming.dta")

# ------------------------------------------------------------------------------
# 2.1: y_hat = 3.194626

model7 <- lm(warmagreecontinuous ~ age + agesq + party7 + white + incomecat + male + educ, data = df2)
summary(model7, robust = TRUE)

# create a data frame with the predictor variables
new_data <- data.frame(age = 35, agesq = 1225, educ = 14, male = 1, white = 1, party7 = 1, incomecat = 3)

# use the predict function to calculate the predicted value
yhat <- predict(model7, new_data)

# print the predicted value
print(yhat)

# ------------------------------------------------------------------------------
# 2.2: Based on the p-values in the model output, we can be confident that party
# affiliation, education level, and age (at a borderline significance level) 
# have an effect on warming opinion. The variables of income category, gender, 
# and race do not appear to have a significant effect. The results are not 
# necessarily surprising, as previous research has shown that party affiliation 
# and education level are strong predictors of attitudes towards climate change. 
# Age has also been found to be a significant factor in shaping attitudes 
# towards climate change, with younger people generally being more concerned 
# about the issue. But, in this model, it seems as if age has a negative 
# relationship with the depedent variable which we expect but that is not 
# signifincant. But, agesq has a positive relationship and is not what we would 
# expect. Additionally, it is worth noting that while race and gender may not have a 
# significant effect in this particular model, there may be other factors 
# related to these variables that could influence attitudes towards climate 
# change.

# ------------------------------------------------------------------------------
# 2.3: To test the significance of adding the agesq variable, we can use an 
# F-test comparing the full model (model8) to a reduced model (model8_reduced) 
# that excludes agesq. The null hypothesis of the F-test is that the reduced 
# model fits the data just as well as the full model, while the alternative 
# hypothesis is that the full model provides a better fit. In R, we can use the 
# anova() function with the two models as arguments to obtain the F-statistic 
# and p-value for the test. From the output, we see that the p-value for the 
# F-test is 0.03547, which is below the conventional threshold of 0.05. 
# Therefore, we reject the null hypothesis and conclude that adding the agesq 
# variable significantly improves the fit of the model.

model8 <- lm(warmagreecontinuous ~ age + agesq + party7 + white + incomecat + male + educ, data = df2)

model8_reduced <- lm(warmagreecontinuous ~ age + party7 + white + incomecat + male + educ, data = df2)

anova(model8_reduced, model8)

# ------------------------------------------------------------------------------
# 2.3(bonus): Looking at the regression table, we see that age has a negative 
# relationship while agesq has a positive one. Also, age is not significant 
# while agesq is. This is a sign that there might be a polynomial relationship.

# ------------------------------------------------------------------------------
# 2.4: Based on the quadratic model in which age and age squared are included as
# predictors, the level of belief in climate change is predicted to be at a 
# minimum when a person is approximately 43 years old (holding other variables 
# constant).

a <- coef(model8)[["agesq"]]
b <- coef(model8)[["age"]]
x0 <- -b / (2*a)
y0 <- a*x0^2 + b*x0 + coef(model8)[["(Intercept)"]]
vertex <- data.frame(x0, y0)

# Print the vertex
vertex

# ------------------------------------------------------------------------------
# QUESTION 3

data1 <- read_dta("social_welfare_race(1).dta")

data2 <- read.csv("State House Partisanship 2010-22 - Sheet1.csv")

df3 <- merge(data1, data2[, c("state", "tot_avg")], by = "state")
# ------------------------------------------------------------------------------
# 3.1: Yes.
# 3.1(a): Looks curvilinear, almost like a u-shaped parabola.
# 3.1(b): Looks curvilinear and maybe even a little logarithmic as well.
# 3.1(c): Looks mostly linear with a slight logarithmic look. 

df3 <- df3[df3$state != "Alaska",]

# regression model
model9 <- lm(welfare3 ~ tot_avg + minority_share + minwage_annual, data = df3)
summary(model9)

# scatterplot of welfare benefit against total average Republican share
ggplot(df3, aes(x = scale(tot_avg), y = welfare3)) +
  geom_point() +
  labs(x = "Total Average Republican Share (Scaled)", y = "Maximum Monthly Welfare Payment for Single Parent with Two Children")

# scatterplot of welfare benefit against minority share
ggplot(df3, aes(x = scale(minority_share), y = welfare3)) +
  geom_point() +
  labs(x = "Minority Share (Scaled)", y = "Maximum Monthly Welfare Payment for Single Parent with Two Children")

# scatterplot of welfare benefit against minimum wage
ggplot(df3, aes(x = scale(minwage_annual), y = welfare3)) +
  geom_point() +
  labs(x = "Minimum Wage (Annual, Scaled)", y = "Maximum Monthly Welfare Payment for Single Parent with Two Children")

# ------------------------------------------------------------------------------
# 3.2: Estimating many models using every possible combination of linear, 
# logged, or polynomial transformations for the independent variables is not 
# considered "best practice" in data analysis because it increases the chances 
# of finding a spurious relationship by chance alone. This is also known as the 
# problem of multiple comparisons. When testing many models, it is more likely 
# that one of them will show a statistically significant result purely by 
# chance, even if there is no real relationship between the independent and 
# dependent variables. This is why it is important to have a clear hypothesis 
# and a priori reasoning for choosing specific transformations or combinations 
# of independent variables to include in a regression model. This approach will 
# help reduce the risk of spurious results and improve the reliability of the 
# analysis.

# ------------------------------------------------------------------------------
# 3.2:
# What I decided to do for each:
# Republican dominance - quadratic transformation (u-shaped parabola)
# Racial composition - logarithmic transformation
# Average wage - no transformation (mostly linear with a slight logarithmic look)


# quadratic transformation for tot_avg
df3$tot_avg_quad <- df3$tot_avg^2

# logarithmic transformation for minority_share
df3$minority_share_log <- log(df3$minority_share)

# linear model with slight logarithmic look for minwage_annual
model10 <- lm(welfare3 ~ tot_avg + tot_avg_quad + minority_share_log + minwage_annual, data = df3)
summary(model10)

# ------------------------------------------------------------------------------
# 3.4: The new model includes transformations of the variables tot_avg and 
# minority_share, and an additional quadratic term for tot_avg. The transformed 
# variables are tot_avg_quad (quadratic term for tot_avg) and minority_share_log 
# (logarithmic transformation of minority_share).

# The coefficients of the model suggest that there is a statistically 
# significant relationship between the maximum monthly welfare payment and the 
# variables tot_avg, tot_avg_quad, and minority_share_log. The variable 
# minwage_annual, on the other hand, is not statistically significant.

# Interpreting the coefficients, we can see that an increase in tot_avg is 
# associated with a decrease in the maximum monthly welfare payment. However, 
# this relationship is not linear, as the quadratic term tot_avg_quad suggests a
# curvilinear effect of tot_avg on welfare payment. An increase in 
# minority_share_log is associated with a decrease in the maximum monthly 
# welfare payment.

# The R-squared value of the new model (0.5647) is higher than that of the 
# original model (0.475), indicating that the new model explains more of the 
# variance in the data than the original model. This suggests that the 
# transformations applied to the variables in the new model have improved the 
# fit of the model to the data.

# Overall, the results suggest that Republican dominance 
# (represented by tot_avg) and racial composition 
# (represented by minority_share) have a significant impact on the maximum 
# monthly welfare payment, with a curvilinear effect of tot_avg. The minimum 
# wage, however, does not appear to be significantly related to the maximum 
# monthly welfare payment.

# ------------------------------------------------------------------------------
# QUESTION 4

data3 <- read_dta("updatedctsolar22.dta")
df4 <- data3[-1, ]

# What I decided to do for each:
# pop - logarithmic transformation
# hhmedincome2020 - quadratic transformation
# distressed - no transformation (binary)
# pop_density - no transformation (mostly linear)
# votefortrump2020 - no transformation (binary)

# The new model with the transformed variables 
# (log_pop and hhmedincome2020_quad) appears to be a better specified model as 
# it has a higher adjusted R-squared value (0.6953) compared to the original 
# model (0.6402) and all of its variables (except votefortrump) are 
# statistically significant. Additionally, the residual standard error of the 
# new model is lower than thatof the old model, indicating that the new model 
# provides a better fit to the data. Therefore, the new model is preferred over 
# the old model.

# estimate the initial linear regression model
model11 <- lm(solar_systems ~ pop + hhmedincome2020 + distressed + pop_density + votefortrump2020, data = df4)

# save the results
summary(model11)

# scatterplot of solar_systems against pop
ggplot(df4, aes(x = pop, y = solar_systems)) +
  geom_point()

# scatterplot of solar_systems against hhmedincome2020
ggplot(df4, aes(x = hhmedincome2020, y = solar_systems)) +
  geom_point()

# scatterplot of solar_systems against hhmedincome2020
ggplot(df4, aes(x = distressed, y = solar_systems)) +
  geom_point()

# scatterplot of solar_systems against pop_density
ggplot(df4, aes(x = pop_density, y = solar_systems)) +
  geom_point()

# scatterplot of solar_systems against votefortrump2020
ggplot(df4, aes(x = votefortrump2020, y = solar_systems)) +
  geom_point()

# Create the transformed variables
df4$log_pop <- log(df4$pop)
df4$hhmedincome2020_recip <- 1/df4$hhmedincome2020
df4$hhmedincome2020_quad <- df4$hhmedincome2020^2


# Estimate the linear model with transformed variables
model12 <- lm(solar_systems ~ log_pop + hhmedincome2020 +  hhmedincome2020_quad + distressed + pop_density + votefortrump2020, data = df4)

summary(model12)
# ------------------------------------------------------------------------------


